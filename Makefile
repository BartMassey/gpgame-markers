# Copyright © 2014 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.


OUTPUTS = front.svg back.svg numbers.svg

$(OUTPUTS): gpg-markers.py
	for i in $(OUTPUTS); do python3 ./gpg-markers.py "`basename $$i .svg`" >$$i ; done
