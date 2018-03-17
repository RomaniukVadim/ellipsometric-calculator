#!/usr/env python3

import argparse
import math


def main():
    parser = argparse.ArgumentParser(description='Ellipsometry calculator (console version).')
        parser.add_argument("--x2", dest='zname', type=argparse.FileType('r'), help='specify zipfile')
    parser.add_argument("-d", dest='dname', type=argparse.FileType('r'), help='specify dictionary file')
    args = parser.parse_args()
    print("")


if __name__ == '__main__':
    main()
