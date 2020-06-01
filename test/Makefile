# Author: Samuele Favazza <sfavazza.github@gmail.com>
# Copyright (C) Samuele Favazza
# 
# Purpose: a Makefile to conveniently run all test cases in batch mode
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

test_files := $(wildcard *.el)

.PHONY: all $(test_files)

all: $(test_files)

$(test_files):
	emacs -batch -L ../ -L . -l ert -l $@ -f ert-run-tests-batch-and-exit
