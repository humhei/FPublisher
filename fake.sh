#!/usr/bin/env bash
ls
dotnet restore build.proj
dotnet fake $@