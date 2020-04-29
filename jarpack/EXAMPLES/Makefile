
DIRS=BAND NONSYM SIMPLE SVD SYM 

examples:
	@( \
	for f in $(DIRS); \
	do \
		cd $$f; \
		echo Making in $$f; \
		$(MAKE) ; \
		cd ..; \
	done );

clean:
	@( \
	for f in $(DIRS); \
	do \
		cd $$f; \
		echo Making clean in $$f; \
		$(MAKE) clean; \
		cd ..; \
	done );
