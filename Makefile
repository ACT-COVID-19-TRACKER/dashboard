########################################################################
#
# Generic Makefile
#
# Copyright (c) Graham.Williams@togaware.com
#
# License: Creative Commons Attribution-ShareAlike 4.0 International.
#
########################################################################

APP=dashboard
VER=0.0.0

INC_BASE    = $(HOME)/.local/share/make
INC_CLEAN   = $(INC_BASE)/clean.mk
INC_R       = $(INC_BASE)/r.mk
INC_KNITR   = $(INC_BASE)/knitr.mk
INC_PANDOC  = $(INC_BASE)/pandoc.mk
INC_GIT     = $(INC_BASE)/git.mk
INC_AZURE   = $(INC_BASE)/azure.mk
INC_LATEX   = $(INC_BASE)/latex.mk
INC_DOCKER  = $(INC_BASE)/docker.mk
INC_MLHUB   = $(INC_BASE)/mlhub.mk

ifneq ("$(wildcard $(INC_CLEAN))","")
  include $(INC_CLEAN)
endif
ifneq ("$(wildcard $(INC_R))","")
  include $(INC_R)
endif
ifneq ("$(wildcard $(INC_KNITR))","")
  include $(INC_KNITR)
endif
ifneq ("$(wildcard $(INC_PANDOC))","")
  include $(INC_PANDOC)
endif
ifneq ("$(wildcard $(INC_GIT))","")
  include $(INC_GIT)
endif
ifneq ("$(wildcard $(INC_AZURE))","")
  include $(INC_AZURE)
endif
ifneq ("$(wildcard $(INC_LATEX))","")
  include $(INC_LATEX)
endif
ifneq ("$(wildcard $(INC_DOCKER))","")
  include $(INC_DOCKER)
endif
ifneq ("$(wildcard $(INC_MLHUB))","")
  include $(INC_MLHUB)
endif

define HELP
Local:

  target	Description
  target	Description

endef
export HELP

help::
	@echo "$$HELP"

