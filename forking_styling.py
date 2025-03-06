import re
import sys
from styling import KEYWORDS

def lowercase_keywords(file):
    print("Lowercasing keywords in", file)
    with open(file, 'r') as fp:
        lines = fp.read()
        for keyword in KEYWORDS:
            pattern = rf"\b{re.escape(keyword)}\b"
            lines = re.sub(pattern, convert_to_lower, lines)
    with open(file, 'w') as fp:
        for line in lines:
            fp.write(line)

def main():


if __name__ == '__main__':
    main()
