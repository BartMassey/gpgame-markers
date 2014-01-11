# Copyright © 2014 Bart Massey

OUTPUTS = front.svg back.svg numbers.svg

$(OUTPUTS): gpg-markers.py
	for i in $(OUTPUTS): do python3 ./gpg-markers.py "`basename $$i .svg`" >$$i ; done
