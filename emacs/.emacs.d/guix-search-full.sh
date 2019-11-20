#!/bin/sh
guix package -s $@ | grep ""
