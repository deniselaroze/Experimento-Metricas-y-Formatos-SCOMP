

rm(list = ls())

args <- commandArgs(TRUE)

args<-c("F", "nivel1", "Retiro Programado", "Renta Vitalicia Inmediata simple", "b")

gender<-args[1]   ## G�nero
econ<-args[2]    ## SES
mode1Q<-args[3] ## primera selecci�n modalidad
mode2Q<-args[4] ## segunda selecci�n modalidad
pg<-args[5]

source("Treatment-Script.R")
