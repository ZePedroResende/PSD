#!/usr/bin/env bash

http post localhost:8080/exchanges name==exch1 host==localhost port==1234 --json -v
http post localhost:8080/companies name==Amazon exchange==exch1 --json -v

