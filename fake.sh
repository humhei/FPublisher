#!/usr/bin/env bash
mono --version
dotnet restore build.proj
dotnet fake $@