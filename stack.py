#!/usr/bin/env python3

def one(s):
    raise s

def two(s):
    one(s)

def three():
    two("my message")

three()
