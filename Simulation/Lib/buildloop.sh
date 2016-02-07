#!/bin/bash

stack clean
stack test --file-watch --coverage
