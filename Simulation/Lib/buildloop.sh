#!/bin/bash

stack clean --resolver=lts
stack test --file-watch --coverage --resolver=lts
