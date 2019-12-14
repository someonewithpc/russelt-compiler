#!/bin/bash

make && ./compiler $1 -i | pr -mt $1 -
