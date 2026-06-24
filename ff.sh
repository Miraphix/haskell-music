#!/bin/bash

ffmpeg -f f32le -ar 48000 -i output.bin output.mp3 && cp ./output.mp3 ~/desktop/winDesktop
