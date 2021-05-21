
all:
	@

lab/%:
	LAB="lab$(@F)"; \
	if [ ! -d "$$LAB" ]; then \
		wget https://cs61a.org/lab/$$LAB/$$LAB.zip && \
		unzip $$LAB.zip && rm $$LAB.zip; \
	fi

hw/%:
	HW="hw$(@F)"; \
	if [ ! -d "$$HW" ]; then \
		wget https://cs61a.org/hw/$$HW/$$HW.zip && \
		unzip $$HW.zip && rm $$HW.zip; \
	fi
