#!/bin/sh 

sudo rm -rf /Users/mac703/*
zpool destroy tank
sudo zpool create tank /tmp/dysk