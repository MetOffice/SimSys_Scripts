#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################


import os
import argparse


def check_path(opt):
    """
    Check that a command line supplied path exists
    """
    if not os.path.exists(opt):
        raise argparse.ArgumentTypeError(
            f"The path '{opt}' does not exist. Command line supplied paths "
            "must be valid."
        )
    return opt


def parse_args():
    """
    Read command line args
    """

    parser = argparse.ArgumentParser(
        "Convert a rose-app.conf file into a namelist file."
    )
    parser.add_argument(
        "input_file",
        type=check_path,
        help="The path to the rose-app.conf file being converted.",
    )
    parser.add_argument(
        "-o",
        "--output_file",
        default=None,
        type=check_path,
        help="The path to an output file. If not given, the output will be "
        "written to 'configuration.nml' in the same location as the input file."
    )
    return parser.parse_args()


def conf_to_nml(input_file, output_file=None):
    """
    Main Function for this program
    Inputs:
        - input_file path, required
        - output_file path, optional, defaults to 'configuration.nml' in same
          directory as input file
    """

    # Set Default Output File
    if not output_file:
        output_file = os.path.join(os.path.dirname(input_file), "configuration.nml")


if __name__ == "__main__":
    args = parse_args()
    conf_to_nml(args.input_file, args.output_file)
