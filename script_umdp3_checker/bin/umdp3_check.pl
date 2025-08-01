#!/usr/bin/env perl
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

# Script to check whether a code change complies with UMDP 003
# Basically the 'guts' of the UMDP3.pm class's perform_task method without
# the UTF-isms such as status objects and the OO stuff

use strict;
use warnings;
use 5.010;
use Cwd 'abs_path';
use threads;
use threads::shared;
use List::Util qw(max);
use File::MimeInfo::Magic;
use IO::ScalarArray;

use IPC::Run qw(run);

# Set the location of the UMDP3 package.
use FindBin;
use lib "$FindBin::Bin";

use UMDP3;
use UMDP3CriticPolicy;

# This is a standalone version of the dispatch tables from UMDP3Job, generated
# by script automatically
use UMDP3DispatchTables;

# Declare version - this is the last UM version this script was updated for:
our $VERSION = '13.5.0';

# Declare variables
my $fcm  = '/etc/profile'; # File to source to access 'fcm' commands
my $exit = 0;              # Exit code of this script == number of failing files
my %additions : shared;    # Hash of added code
my %deletions;             # Hash of deleted files
my $filename   = '';       # Current filename being tested
my $snooze     = 120;      # Time to wait before retrying
my $max_snooze = 10;       # Maximum number of retries before aborting

# Shared variables for multi-threading
# Variables with the "shared" attribute become shared memory variables -
# i.e. they are accessable by all threads.
# Other variables are private to each thread.
my @branchls_threads;
my @add_keys_threads;
my @output_threads : shared;
my @exit_threads : shared;

# Get argument from command line, or assume '.' if not defined
my $branch = shift // '.';

# Cope with UTF-style working copy syntax just in case
$branch =~ s/wc://sxm;

# Read text file of whitelisted include files
my $whitelist_includes_file = shift;

unless ( $whitelist_includes_file and -f $whitelist_includes_file ) {
    die "Whitelist filename not provided.\n";
}

# Read in retired if-defs
my @includes = read_file($whitelist_includes_file);

my $suite_mode = 0;
if ( $ENV{SOURCE_UM_MIRROR} ) {
    print "Detected SOURCE_UM_MIRROR environment variable.\n";
    $branch = $ENV{SOURCE_UM_MIRROR};
    print "Redirecting branch to $branch\n";
    $suite_mode = 1;
}

# Set up threads

# Determin the number of threads to use. Default to 1 (i.e. run serially), but override this
# if the UMDP_CHECKER_THREADS environment variable is set.

my $num_threads = 1;
if ( $ENV{UMDP_CHECKER_THREADS} ) {
    $num_threads = $ENV{UMDP_CHECKER_THREADS};
    if ( $num_threads < 1 ) {
        print
          "UMDP_CHECKER_THREADS environment variable is invalid: overriding\n";
        $num_threads = 1;
    }
    print "Using $num_threads threads\n";
}

# Determine cylc logging
my $log_cylc = 0;
if ( $ENV{CYLC_TASK_LOG_ROOT} ) {
    $log_cylc = $ENV{CYLC_TASK_LOG_ROOT};
    print "Using cylc logging directory: $log_cylc\n";
}

# Now we have the number of threads required, set up a threads array, with one entry
# for each thread. This will be used later to hold the control information for each
# thread in use.

my @threads_array;

for ( my $i = 1 ; $i <= $num_threads ; $i++ ) {
    push( @threads_array, $i );
}

my %dispatch_table_diff_fortran =
  UMDP3DispatchTables::get_diff_dispatch_table_fortran();
my %dispatch_table_diff_c = UMDP3DispatchTables::get_diff_dispatch_table_c();
my %dispatch_table_file_c = UMDP3DispatchTables::get_file_dispatch_table_c();
my %dispatch_table_file_all =
  UMDP3DispatchTables::get_file_dispatch_table_all();

my @binfo;
my $binfocode;

my $trunkmode   = 0;
my $error_trunk = 0;

start_branch_checking:

# Check this is a branch rather than the trunk
@binfo     = `. $fcm; fcm binfo $branch 2>&1`;
$binfocode = $?;

unless ( $binfocode == 0 ) {
    if ( grep( /svn\sinfo\s--xml/sxm, @binfo ) ) {
        if ($suite_mode) {
            for ( my $i = 1 ; $i <= $max_snooze ; $i++ ) {
                print
"Revision probably doesn't exist yet - waiting $snooze seconds for mirror to update (Snooze $i of $max_snooze).\n";
                sleep $snooze;
                @binfo     = `. $fcm; fcm binfo $branch 2>&1`;
                $binfocode = $?;
                last if ( $binfocode == 0 );
            }
        }
    }
    if ( $binfocode != 0 ) {
        print "Error running fcm binfo:\n";
        print @binfo;
        die "FCM error";
    }
}

if (   grep( /URL:\ssvn:\/\/[^\/]+\/(\w|\.)+_svn\/\w+\/trunk/sxm, @binfo )
    or grep( /URL:\shttps:\/\/[^\/]+\/svn\/[\w\.]+\/\w+\/trunk/sxm, @binfo )
    or grep( /URL:.*\/svn\/\w+\/main\/trunk/sxm, @binfo )
    or grep( /URL:..*_svn\/main\/trunk/sxm,      @binfo )
    or grep( /URL:\sfile:\/\/.*\/trunk/sxm,      @binfo ) )
{
    print "Detected trunk: checking full source tree\n";
    $branch =~ s/@.*$//sxm;
    $trunkmode = 1;
    if ( $ENV{UMDP_CHECKER_TRUNK_ERROR} ) {
        $error_trunk = $ENV{UMDP_CHECKER_TRUNK_ERROR};
        if ( $error_trunk == 1 ) {
            print
"UMDP_CHECKER_TRUNK_ERROR environment variable is set to 1: failures will be fatal\n";
        }
        elsif ( $error_trunk == -1 ) {
            print
"UMDP_CHECKER_TRUNK_ERROR environment variable is set to -1: skipping UMPD3 checks for the trunk\n";
            exit 0;
        }
        else {
            print
"UMDP_CHECKER_TRUNK_ERROR environment variable is set to $error_trunk: failures will be ignored\n";
            print
"Set this to 1 to cause the checker script to exit with an error code on finding failures.\n";
            print
"Alternatively, set this to -1 to cause the checker script to exit without checking the trunk.\n";
        }
    }
    else {
        print "UMDP_CHECKER_TRUNK_ERROR environment variable is not present.\n";
        print
"Set this to 1 to cause the checker script to exit with an error code on finding failures.\n";
        print
"Alternatively, set this to -1 to cause the checker script to exit without checking the trunk.\n";
    }
}

foreach my $line (@binfo) {
    if ( $line =~ m{Branch\sParent:.*/trunk@.*}sxm ) {
        last;
    }
    elsif ( $line =~ m/Branch\sParent:\s*(.*)/sxm ) {
        print "This branch is a branch-of-branch - testing parent ($1)\n";
        $branch = $1;
        goto start_branch_checking;
    }
}

my @info;

# Get fcm info for branch
@info      = `. $fcm; fcm info $branch 2>&1`;
$binfocode = $?;

if ( $binfocode != 0 ) {
    print "Error running fcm info:\n";
    print @info;
    die "FCM error";
}

my $repository_branch_path;
my $repository_working_path;
my $repository_relative_path;

foreach my $line (@binfo) {
    if ( $line =~ /^URL:\s*(.*)/sxm ) {
        $repository_branch_path = $1;
        last;
    }
}

foreach my $line (@info) {
    if ( $line =~ /^URL:\s*(.*)/sxm ) {
        $repository_working_path = $1;
        last;
    }
}

$repository_relative_path = $repository_working_path;
$repository_relative_path =~ s/$repository_branch_path//sxm;
$repository_relative_path =~ s/\n//sxm;

# replace relative branch paths with absolute paths
if ( grep( /Working\sCopy\sRoot\sPath:/sxm, @info ) ) {
    $branch = abs_path($branch);
}

# trim trailing "/"
$branch =~ s{/$}{}sxm;

print "Testing branch $branch\n";

if ( $trunkmode == 0 ) {

    if ($repository_relative_path) {
        print
"\n[WARN] The relative path between the root of the branch and the script working path ($repository_relative_path) is not empty\n";
        print "       - you are not running from the root of the branch\n\n";
        if ($suite_mode) {
            die "Error - re-run from the root of the branch\n";
        }
    }

    # Get the diff
    my @diff     = `. $fcm; fcm bdiff $branch  2>&1`;
    my $diffcode = $?;

    # Check the bdiff worked correctly
    unless ( $diffcode == 0 ) {
        die "Error running 'fcm bdiff $branch':\n@diff\n";
    }

# We will need to know empty and deleted files - use the bdiff summary to identify these.
    my @summary = `. $fcm; fcm bdiff --summarise $branch  2>&1`;
    $diffcode = $?;

    # Check the second bdiff worked correctly
    unless ( $diffcode == 0 ) {
        die "Error running 'fcm bdiff --summarise $branch':\n@summary\n";
    }

    foreach my $line (@summary) {

        # Reset captures to undefined with a trivial successful match.
        "a" =~ /a/sxm;

        # Add hash entries for added or modified files:
        # These are files which are newly added; or which add or remove lines.
        $line =~ /^(A|M+)\s*(?<filename>\S+)$/sxm;
        my $modified_file = $+{filename};
        if ($modified_file) {

            #normalise the path
            $modified_file =~ s/$repository_working_path\///sxm;
            $modified_file =~ s/.*trunk$repository_relative_path\///sxm;

            my @share_arr = [];
            $additions{$modified_file} = share(@share_arr);
        }

        # Reset captures to undefined with a trivial successful match.
        "a" =~ /a/sxm;

        # Add has entries for deleted files
        $line =~ /^D\s*(?<filename>\S+)$/sxm;
        my $deleted_file = $+{filename};
        if ($deleted_file) {

            #normalise the path
            $deleted_file =~ s/$repository_working_path\///sxm;
            $deleted_file =~ s/.*trunk$repository_relative_path\///sxm;
            $deletions{$deleted_file} = [];
        }
    }

    my $store_line = 0;

    # Store the lines added in a hash with the filename as the key, i.e.
    # %additions =  ( 'filename' => [ 'added line 1', 'added line 2'] )
    foreach my $line (@diff) {

        if ( $line =~ /^\+\+\+/sxm ) {

            # Find if the filename is in our additions hash,
            # and set the subsequent lines to be stored if it is.
            $line =~ /^\+\+\+\s+(?<filename>\S+)/sxm;
            $filename = $+{filename};
            unless ( ( $branch eq "." ) || ( $filename eq $branch ) ) {
                $filename =~ s/.*$branch\///sxm;
            }
            $store_line = exists( $additions{$filename} );

            if ( $store_line == 0 ) {

                # if we don't recognise the file as deleted,
                # or as marking an SVN property change,
                # something has gone wrong.
                if ( !exists( $deletions{$filename} ) ) {
                    if (
                        !(
                            grep( /^Property\schanges\son:\s$filename$/sxm,
                                @diff )
                        )
                      )
                    {
                        print "Something has failed parsing line '$line'\n";
                        die
"Filename '$filename' is not contained in the output from fcm bdiff --summarise!\n";
                    }
                }
            }

        }
        elsif ( $line =~ /^\+/sxm ) {
            if ($store_line) {

                # Add the diff to %additions hash
                $line =~ s/^\+//sxm;
                push @{ $additions{$filename} }, $line;
            }
        }
    }

}

# The @external_checks array contains the names of all the non-UM repositories
# extracted by the UM which should also be checked.
my @external_checks = ( "shumlib", "meta", "ukca" );
my %filepath_mapping = ( 'meta' => 'um_meta' );
my @extracts = ();

if ( $trunkmode == 0 ) {
    if ($suite_mode) {

        # enable trunkmode for specific repositories if the environment does
        # not match rose-stem/rose-suite.conf

        my $ss_env     = $ENV{SCRIPT_SOURCE};
        my @suite_conf = cat_file( $ss_env . "/um/rose-stem/rose-suite.conf" );
        my @host_sources = grep /^HOST_SOURCE_.*=/, @suite_conf;

        print "Detected HOST_SOURCE variables:\n";
        print join( "", @host_sources );

        foreach (@external_checks) {
            my $repo   = $_;
            my $o_repo = $repo;
            if ( exists $filepath_mapping{$repo} ) {
                $repo = $filepath_mapping{$repo};
            }
            my $host_var_name = "HOST_SOURCE_" . uc($repo);
            my $env_var_res   = $ENV{$host_var_name};
            if ( !grep /^$host_var_name=(\"|\')$env_var_res(\"|\')/,
                @host_sources )
            {
                print $host_var_name
                  . " modified in environment."
                  . " Running full check on this repository\n";
                push @extracts, $o_repo;
            }
        }

    }

    # enable trunkmode for specific repositories if rose-stem/rose-suite.conf
    # is modified
    if ( exists $additions{"rose-stem/rose-suite.conf"} ) {
        print "rose-stem/rose-suite.conf modified:"
          . " checking for external repository updates\n";
        my $added_lines_ref = $additions{"rose-stem/rose-suite.conf"};
        my @added_lines     = @$added_lines_ref;
        foreach (@external_checks) {
            my $repo   = $_;
            my $o_repo = $repo;
            if ( exists $filepath_mapping{$repo} ) {
                $repo = $filepath_mapping{$repo};
            }
            my $host_var_name = "HOST_SOURCE_" . uc($repo);
            if ( grep /^$host_var_name=/, @added_lines ) {
                print $host_var_name
                  . " modified in rose-suite.conf."
                  . " Running full check on this repository\n";
                push @extracts, $o_repo;
            }
        }
    }

    # remove any duplicates
    my %unique_extracts = map { $_ => 1 } @extracts;
    @extracts = keys %unique_extracts;

    # If we captured any changes, enable trunk-mode for those repositories.
    if ( scalar(@extracts) > 0 ) {
        $trunkmode   = 1;
        $error_trunk = 1;
        unshift @extracts, "";
    }
}
else {
    @extracts = ( "", "um" );
    push @extracts, @external_checks;
}

if ( $trunkmode == 1 ) {

    #trunk mode: cat all the source files to %additions

    my @branchls;
    my $returncode;

    if ($suite_mode) {

        # If we are in suite mode, we need to generate the ls from the extracted
        # sources, not from FCM.

        my $ss_env = $ENV{SCRIPT_SOURCE};
        my $extracts_path = join( " $ss_env/", @extracts );

        print "Using extracted source from path(s) : $extracts_path\n";

        my @exract_source =
          `find $extracts_path -type f -exec readlink -f {} \\; 2>&1`;
        $returncode = $?;

        if ( $returncode != 0 ) {
            die "Error running 'find $extracts_path':\n@exract_source\n";
        }

        my $cs_env = $ENV{CYLC_SUITE_SHARE_DIR};

        $cs_env = `readlink -f $cs_env`;
        chomp $cs_env;

        my @script_source =
`find $cs_env/imported_github_scripts -type f -not -ipath "*/.git/*" -exec readlink -f {} \\; 2>&1`;
        $returncode = $?;

        if ( $returncode != 0 ) {
            die
"Error running 'find $cs_env/imported_github_scripts':\n@script_source\n";
        }

        push( @branchls, @exract_source );
        push( @branchls, @script_source );

        # convert the realtive paths to be relative to the extract location

        if ( $#exract_source >= 0 ) {
            $repository_working_path = $exract_source[0];
        }
        else {
            $repository_working_path = "[ ]";
        }

        $repository_working_path =~ s{/um/.*$}{}sxm;
        $repository_working_path =
          "(" . $cs_env . "|" . $repository_working_path . ")";
        $repository_relative_path = "";

    }
    else {

        @branchls   = `. $fcm; fcm ls -R $branch 2>&1`;
        $returncode = $?;

        unless ( $returncode == 0 ) {
            die "Error running ' fcm ls -R $branch':\n@branchls\n";
        }

    }

    # check there are some files availible to test!
    unless ( $#branchls >= 0 ) {
        die "Error: no files in $branch\n";
    }

    # because the work done by each thread will be unbalanced,
    # we should take a guided approach - therefore split into
    # multiple branchls blocks

    # reduce the number of threads if are too few files
    # (prevent empty threads)
    if ( $#branchls < $num_threads - 1 ) {
        $num_threads = $#branchls + 1;
    }

# Set up the size of the chunk of work each thread will do.
# Each thread should process at least one key (i.e. at least one element of
# the array @branchls, which in turn are used as the keys of the hash %additions)
# However, we also want to balance the work.
# We will start with a chunk size equivalent to a third of the keys divided
# equally across the threads, then progrssively re-use each thread with smaller
# chunks until the entire work pool has been exhausted.

    my $thread_branchls_len;

    $thread_branchls_len = max( 1, ( $#branchls + 1 ) / ( 3 * $num_threads ) );

    # fork the threads to execute trunk_files_parse
    for ( my $i = 0 ; $i < $num_threads ; $i++ ) {
        $branchls_threads[$i] = [];

# Store the work (in this case the list of files to process) for the i'th thread,
# by taking a chunk from the original branchls array. The chunk will be of size
# thread_branchls_len.
        push @{ $branchls_threads[$i] }, splice @branchls,
          $#branchls - $thread_branchls_len + 1;

   # fork the thread
   # This will create a new thread which will execute the trunk_files_parse sub.
   # Its thread id will be stored in the threads array.
        $threads_array[$i] = threads->create( \&trunk_files_parse, $i );
    }

    my @th_l;

    # add the currently running threads to the list
    @th_l = threads->list(threads::running);

    # add the threads which have run work, but have already finished it.
    push @th_l, threads->list(threads::joinable);

    # re-join (and possibly re-fork) all the threads.
    # By doing this we will recycle all the threads until the entire work
    # pool is executed and completed.
    while ( $#branchls >= 0 or $#th_l >= 0 ) {
        for ( my $i = 0 ; $i < $num_threads ; $i++ ) {

     # Check if any of the threads in out list is done with its work chunk.
     # If it is, we can re-join it, then recycle it by issuing a new work chunk.
            if ( $threads_array[$i]->is_joinable() ) {
                my $return_code = $threads_array[$i]->join();
                if ( !defined $return_code ) {
                    print "thread ", $threads_array[$i]->tid(),
                      ": terminated abnormally [A]\n";
                    $exit += 1;
                    $error_trunk = 1;
                }

  # Calculate a new work chunk.
  # This chunk size will get progressivly smaller as the work pool is exhausted.
                $thread_branchls_len =
                  max( 1, ( $#branchls + 1 ) / ( 3 * $num_threads ) );
                if ( $#branchls >= 0 ) {
                    $branchls_threads[$i] = [];

                    # Give the thread a new chunk of work
                    if ( $thread_branchls_len > $#branchls + 1 ) {
                        push @{ $branchls_threads[$i] }, splice @branchls, 0;
                    }
                    else {
                        push @{ $branchls_threads[$i] }, splice @branchls,
                          $#branchls - $thread_branchls_len + 1;
                    }
                    $threads_array[$i] =
                      threads->create( \&trunk_files_parse, $i );
                }
            }
        }

        # Update the list of threads.
        @th_l = threads->list(threads::running);
        push @th_l, threads->list(threads::joinable);
    }

    # By this point we have allocated all the work pool to the threads.
    # Check all threads are re-joined - this will finalise the threads,
    # and will block execution for any threads still processing work
    # until they have completed it.

    foreach my $thread ( threads->list() ) {
        my $return_code = $thread->join();
        if ( !defined $return_code ) {
            print "thread ", $thread->tid(), ": terminated abnormally [B]\n";
            $exit += 1;
            $error_trunk = 1;
        }
    }

}

# Set up the error message string to empty
my $message = '';

# set up known includes whitelist
my %includes_hash;
@includes_hash{@includes} = ();

my @add_keys = keys %additions;

# only run checks if there is at least one file to check
if ( $#add_keys >= 0 ) {

    # reduce the number of threads if are too few keys
    # (prevent empty threads)
    if ( $#add_keys < $num_threads - 1 ) {
        $num_threads = $#add_keys + 1;
    }

 # Set up the size of the chunk of work each thread will do.
 # Each thread should process at least one key (i.e. at least one element of
 # the array @add_keys, which in turn are the keys of the hash %additions)
 # However, we also want to balance the work.
 # We will start with a chunk size equivalent to a third of the keys divided
 # equally across the threads, then progrssively re-use each thread with smaller
 # chunks until the entire work pool has been exhausted.

    my $thread_add_keys_len;

    $thread_add_keys_len = max( 1, ( $#add_keys + 1 ) / ( 3 * $num_threads ) );

    # fork the threads to execute run_checks
    for ( my $i = 0 ; $i < $num_threads ; $i++ ) {

        # Initialise a shared memory space to store the output from each thread.
        # This is shared so the main thread will be able to retrieve the output.
        my @share_arr = [];
        $output_threads[$i] = share(@share_arr);

        $add_keys_threads[$i] = [];

# Store the work (in this case the list of added keys to process) for the i'th thread,
# by taking a chunk from the original add_keys array. The chunk will be of size
# thread_add_keys_len.
        push @{ $add_keys_threads[$i] }, splice @add_keys,
          $#add_keys - $thread_add_keys_len + 1;

        $exit_threads[$i] = 0;

        # fork the thread
        # This will create a new thread which will execute the run_checks sub.
        # Its thread id will be stored in the threads array.
        $threads_array[$i] = threads->create( \&run_checks, $i );
    }

    # Create a list of threads - th_l - which contains those threads that are
    # currently doing, or have done, some work. These are the threads which
    # we will have to untimately finalise, possibly after waiting for them to
    # complete.

    my @th_l;

    # add the currently running threads to the list
    @th_l = threads->list(threads::running);

    # add the threads which have run work, but have already finished it.
    push @th_l, threads->list(threads::joinable);

    # re-join (and possibly re-fork) all the threads.
    # By doing this we will recycle all the threads until the entire work
    # pool is executed and completed.
    while ( $#add_keys >= 0 and $#th_l >= 0 ) {
        for ( my $i = 0 ; $i < $num_threads ; $i++ ) {

     # Check if any of the threads in out list is done with its work chunk.
     # If it is, we can re-join it, then recycle it by issuing a new work chunk.
            if ( $threads_array[$i]->is_joinable() ) {
                my $return_code = $threads_array[$i]->join();
                if ( !defined $return_code ) {
                    print "thread ", $threads_array[$i]->tid(),
                      ": terminated abnormally [C]\n";
                    $exit += 1;
                    $error_trunk = 1;
                }

  # Calculate a new work chunk.
  # This chunk size will get progressivly smaller as the work pool is exhausted.
                $thread_add_keys_len =
                  max( 1, ( $#add_keys + 1 ) / ( 3 * $num_threads ) );
                $exit += $exit_threads[$i];
                $exit_threads[$i] = 0;
                if ( $#add_keys >= 0 ) {
                    $add_keys_threads[$i] = [];

                    # Give the thread a new chunk of work
                    if ( $thread_add_keys_len > $#add_keys + 1 ) {
                        push @{ $add_keys_threads[$i] }, splice @add_keys, 0;
                    }
                    else {
                        push @{ $add_keys_threads[$i] }, splice @add_keys,
                          $#add_keys - $thread_add_keys_len + 1;
                    }
                    $threads_array[$i] = threads->create( \&run_checks, $i );
                }
            }
        }

        # Update the list of threads.
        @th_l = threads->list(threads::running);
        push @th_l, threads->list(threads::joinable);
    }

    # By this point we have allocated all the work pool to the threads.
    # Check all threads are re-joined - this will finalise the threads,
    # and will block execution for any threads still processing work
    # until they have completed it.

    foreach my $thread ( threads->list() ) {
        my $return_code = $thread->join();
        if ( !defined $return_code ) {
            print "thread ", $thread->tid(), ": terminated abnormally [D]\n";
            $exit += 1;
            $error_trunk = 1;
        }
    }

    # Include any previously uncounted failures.
    for ( my $i = 0 ; $i < $num_threads ; $i++ ) {
        $exit += $exit_threads[$i];
    }

    if ( $exit > 0 ) {

  # This section prints failure messages for each file after each file is tested
        print "The following files have failed the UMDP3 compliance tests:\n";
        for ( my $i = 0 ; $i < $num_threads ; $i++ ) {

            # Print the output from each thread in turn.
            print @{ $output_threads[$i] };
        }
    }

}

# Print message for a success if no files have failed
if ( $exit == 0 ) {
    print "No modified files appear to have failed the compliance tests\n";
}
else {
    print "\n[ERROR] There were a total of $exit compliance tests failures\n";
}

# Exit with an exit code dependant on the options chosen.
if ( ( $error_trunk == 1 ) or ( $trunkmode == 0 ) ) {

    # Exit with number of fails, if it's zero (a UNIX success) it passed
    exit( $exit > 0 );
}
else {
    # We are in trunkmode but error_trunk is not set: exit with success
    exit 0;
}

############################### SUBROUTINES ###################################

sub trunk_files_parse {

    foreach my $line ( @{ $branchls_threads[ $_[0] ] } ) {

        #strip newline character
        $line =~ s/\R$//sxm;

        # ignore non-source files
        if ( $line !~ /\/$/sxm ) {

            # Add hash entries for added or modified files:
            my $modified_file = $line;

            #normalise the path
            $modified_file =~ s/$repository_working_path\///sxm;
            $modified_file =~ s/.*trunk$repository_relative_path\///sxm;

            my @share_arr = [];
            $additions{$modified_file} = share(@share_arr);

            my $file_url;

            if ($suite_mode) {
                $file_url = $line;
            }
            else {
                $file_url = "$branch/$modified_file";
            }

            my @file_lines = cat_file($file_url);

            # Store the lines added in a hash with the filename as the key, i.e.
            # %additions =  ( 'filename' => [ 'added line 1', 'added line 2'] )
            push @{ $additions{$modified_file} }, @file_lines;
        }
    }

    # empty return is important for thread return code checking
    return 0;
}

sub run_checks {

    # Loop over modified files

    foreach my $modified_file ( @{ $add_keys_threads[ $_[0] ] } ) {

        # Initialise variables
        my $failed = 0;
        my @failed_tests;
        my $is_c_file               = 0;
        my $is_fortran_include_file = 0;

     # If it's an include file, fail unless it's on the include whitelist
     # (e.g. its a C header or a Fortran include for reducing code duplication).
        if ( $modified_file =~ /\.h$/sxm ) {

            if ( exists( $includes_hash{$modified_file} ) ) {
                my @components = split( "/", $modified_file );
                if (    $components[0] =~ /src/sxm
                    and $components[-2] =~ /include/sxm
                    and not $components[1] =~ /include/sxm )
                {
                    $is_fortran_include_file = 1;
                }
                elsif ( $components[0] =~ /src/sxm
                    and $components[1] =~ /include/sxm )
                {
                    $is_c_file = 1;
                }
                else {
                    push @failed_tests,
"Added an include file outside of a recognised 'include' directory";
                }
            }
            else {
                push @failed_tests,
"Modified or created non-whitelisted include file rather than using a module";
                $failed++;
            }
        }

        if ( $modified_file =~ /\.c$/sxm ) {
            $is_c_file = 1;
        }

        # if it's Fortran or C apply all the tests
        if (   $modified_file =~ /\.F90$/sxm
            or $modified_file =~ /\.f90$/sxm
            or $is_c_file
            or $is_fortran_include_file )
        {

            my $dispatch_table_diff;
            my $dispatch_table_file;

            if ($is_c_file) {
                $dispatch_table_diff = \%dispatch_table_diff_c;
                $dispatch_table_file = \%dispatch_table_file_c;
            }
            else {
                $dispatch_table_diff = \%dispatch_table_diff_fortran;
                my %dispatch_table_file_fortran =
                  UMDP3DispatchTables::get_file_dispatch_table_fortran(
                    $modified_file);
                $dispatch_table_file = \%dispatch_table_file_fortran;
            }

            # Get the diff for this file out of the hash
            my $added_lines_ref = $additions{$modified_file};
            my @added_lines     = @$added_lines_ref;

            # Loop over each test which works on a diff
            foreach my $testname ( keys %$dispatch_table_diff ) {
                UMDP3::reset_extra_error_information();

          # Get the subroutine reference from the tables at the top of this file
                my $subroutine_ref = ${$dispatch_table_diff}{$testname};

                # Run the test
                my $answer = &$subroutine_ref(@added_lines);

                my %extra_error = UMDP3::get_extra_error_information();
                if ( scalar keys %extra_error > 0 ) {
                    my @extra_error = keys %extra_error;
                    my $extra_text = join( ", ", @extra_error );
                    $testname .= ": $extra_text";
                }

       # If the test fails, increase the number of failures and add the testname
       # to the array containing the list of problems with this file
                if ($answer) {
                    $failed++;
                    push @failed_tests, $testname;
                }
            }

            # Get the whole file contents
            # (if we are in trunk mode, @added_lines is already the full file)
            my @file_lines;

            if ( $trunkmode == 1 ) {
                @file_lines = @added_lines;
            }
            else {
    # Analyse the command line argument to work out how to access the whole file
                my $file_url;
                my $url_revision;
                my $short_branch = $branch;
                if ( $short_branch =~ /@/sxm ) {
                    $short_branch =~ s/(@.*)//sxm;
                    $url_revision = $1;
                }

             # The $url_revision variable is only present if the URL is a branch
                if ($url_revision) {
                    $file_url = "$short_branch/$modified_file$url_revision";
                }
                else {
                    $file_url = "$short_branch/$modified_file";
                }

                @file_lines = cat_file($file_url);
            }

          # Perform each test which checks the whole file in a similar method to
          # tests which work on a diff
            foreach my $testname ( keys %$dispatch_table_file ) {
                UMDP3::reset_extra_error_information();
                my $subroutine_ref = ${$dispatch_table_file}{$testname};
                my $answer         = &$subroutine_ref(@file_lines);
                my %extra_error    = UMDP3::get_extra_error_information();
                if ( scalar keys %extra_error > 0 ) {
                    my @extra_error = keys %extra_error;
                    my $extra_text = join( ", ", @extra_error );
                    $testname .= ": $extra_text";
                }
                if ($answer) {
                    $failed++;
                    push @failed_tests, $testname;
                }
            }

            # Perform universal tests
            foreach my $testname ( keys %dispatch_table_file_all ) {
                UMDP3::reset_extra_error_information();
                my $subroutine_ref = $dispatch_table_file_all{$testname};
                my $answer         = &$subroutine_ref(@file_lines);
                my %extra_error    = UMDP3::get_extra_error_information();
                if ( scalar keys %extra_error > 0 ) {
                    my @extra_error = keys %extra_error;
                    my $extra_text = join( ", ", @extra_error );
                    $testname .= ": $extra_text";
                }
                if ($answer) {
                    $failed++;
                    push @failed_tests, $testname;
                }
            }

            # end Filename matches F90/f90/c
        }
        else {
            # Get the whole file contents
            # (if we are in trunk mode, @added_lines is already the full file)
            my @file_lines;

            # Get the diff for this file out of the hash
            my $added_lines_ref = $additions{$modified_file};
            my @added_lines     = @$added_lines_ref;

            if ( $trunkmode == 1 ) {
                @file_lines = @added_lines;
            }
            else {
    # Analyse the command line argument to work out how to access the whole file
                my $file_url;
                my $url_revision;
                my $short_branch = $branch;
                if ( $short_branch =~ /@/sxm ) {
                    $short_branch =~ s/(@.*)//sxm;
                    $url_revision = $1;
                }

             # The $url_revision variable is only present if the URL is a branch
                if ($url_revision) {
                    $file_url = "$short_branch/$modified_file$url_revision";
                }
                else {
                    $file_url = "$short_branch/$modified_file";
                }

                @file_lines = cat_file($file_url);
            }

            # read in data from file to $data, then
            my $io_array = IO::ScalarArray->new(\@file_lines);
            my $mimetype = mimetype($io_array);

            # if we can't detect a mime type, try some tricks to aid detection
            if ( $mimetype =~ /text\/plain/sxm ) {
                my @mime_file_lines = grep !/^\s*\#/sxm, @file_lines;
                $io_array = IO::ScalarArray->new(\@mime_file_lines);
                $mimetype = mimetype($io_array);
            }

            # The binary files array contains all the binary mime types
            # present in the UM.
            my @binary_files = (
                'application/x-tar', 'application/octet-stream',
                'image/gif',         'image/png',
            );

            # Exclude binary formats from universal tests
            if ( !( $mimetype ~~ @binary_files ) ) {

                # Perform universal tests
                foreach my $testname ( keys %dispatch_table_file_all ) {
                    UMDP3::reset_extra_error_information();
                    my $subroutine_ref = $dispatch_table_file_all{$testname};
                    my $answer         = &$subroutine_ref(@file_lines);
                    my %extra_error    = UMDP3::get_extra_error_information();
                    if ( scalar keys %extra_error > 0 ) {
                        my @extra_error = keys %extra_error;
                        my $extra_text = join( ", ", @extra_error );
                        $testname .= ": $extra_text";
                    }
                    if ($answer) {
                        $failed++;
                        push @failed_tests, $testname;
                    }
                }
            }

            my $is_python = 0;
            my $is_perl   = 0;
            my $is_shell  = 0;

            if (   $mimetype =~ /text\/x-python/sxm
                or $modified_file =~ /\.py$/sxm )
            {
                $is_python = 1;
            }

            if ( $mimetype =~ /application\/x-shellscript/sxm ) {
                $is_shell = 1;
            }

            if (   $mimetype =~ /application\/x-perl/sxm
                or $modified_file =~ /\.pl$/sxm
                or $modified_file =~ /\.pm$/sxm )
            {
                $is_perl = 1;
            }

            if ($is_python) {
                my $in  = join "", @file_lines;
                my $out = "";
                my $err = "";

                my $shellcheck = run [ 'pycodestyle', '-' ], \$in, \$out, \$err;

                if ( !$shellcheck ) {
                    $failed++;
                    my $shellcheck_fails = $out . $err;
                    $shellcheck_fails =~ s{\n?\n}{\n  }sxmg;
                    $shellcheck_fails =~ s/stdin:/line /sxmg;
                    push @failed_tests, $shellcheck_fails;
                }
            }

            if ($is_perl) {
                my $critic     = UMDP3CriticPolicy::get_umdp3_critic_policy();
                my $in         = join "", @file_lines;
                my @violations = $critic->critique( \$in );

                if (@violations) {
                    $failed++;
                    my $testname = join "  ", @violations;
                    push @failed_tests, $testname;
                }
            }

            if ($is_shell) {
                my $in  = join "", @file_lines;
                my $out = "";
                my $err = "";

                my $shellcheck = run [ 'shellcheck', '-' ], \$in, \$out, \$err;

                if ( !$shellcheck ) {
                    $failed++;
                    my $shellcheck_fails = $out . $err;
                    $shellcheck_fails =~ s{\n?\n}{\n  }sxmg;
                    $shellcheck_fails =~ s/\s\sIn\s-\s/  /sxmg;
                    push @failed_tests, $shellcheck_fails;
                }
            }

        }

        # If any tests failed, print the failure message
        if ( $failed > 0 ) {
            my $failure_text = join( "\n  ", @failed_tests );

         # The space before the colon makes the filename easier to cut and paste
            $message .= "File $modified_file :\n  $failure_text\n";
            push @{ $output_threads[ $_[0] ] }, $message;
            $exit_threads[ $_[0] ] += $failed;
            if ($log_cylc) {
                my $filename = $modified_file;
                $filename =~ s/\//+/sxmg;
                if ( index( $filename, "." ) != -1 ) {
                    $filename .= "_";
                }
                else {
                    $filename .= ".";
                }
                $filename = $log_cylc . "." . $filename . "report";
                my $fileres = open( my $fh, '>', $filename );
                if ( !defined $fileres ) {
                    die "ERR: $filename\n";
                }
                print $fh $failure_text;
                close($fh);
            }
        }
        $message = '';

    }    # Loop over files

    # empty return is important for thread return code checking
    return 0;
}

# Cat a file, either from fcm (if the URL contains a colon) or from disk
sub cat_file {
    my $url = shift;
    my @lines;
    my $error = 0;

  # If the URL contains a colon treat it as an fcm, else treat as a regular file
    if ( $url =~ /:/sxm ) {
        @lines = `. $fcm; fcm cat $url 2>&1`;
        $error = $?;
    }
    else {
        @lines = `cat $url 2>&1`;
        $error = $?;
    }

    # If there is an error, check if this is not due to the 'node kind'
    # being innappropriate
    if ( $error != 0 ) {
        @lines = `. $fcm; fcm info $url 2>&1`;
        if ( $? == 0 ) {
            if ( ( join "\n", @lines ) !~ /Node\sKind:\sfile/sxmgi ) {
                @lines = ('');
                $error = 0;
            }
        }
    }

    if ($error) {
        die "Error cating file $url\n";
    }

    return @lines;
}

sub read_file {
    my $file = shift;
    open( my $fh, '<', $file ) or die "Cannot read $file: $!\n";
    chomp( my @lines = <$fh> );
    close $fh;
    return @lines;
}
