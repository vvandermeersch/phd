:: Batch script to launch GWGEN from MINGW64 shell (both must be installed)
:: author : V. Van der Meersch - 13/10/2022
@echo off
set infile=%1
set outfile=%2
set outdir=%3
set MSYS_ROOT=C:\msys64
%MSYS_ROOT%\msys2_shell.cmd -mingw64  -c "cd C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/scripts/daily_weather_generator/gwgen_f90-master && ./weathergen %infile% %outfile% && echo '1' > %outdir%/check.temp || echo '0' > %outdir%/error.temp"