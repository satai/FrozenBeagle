FrozenBeagle: simulation simulation-results pdf

clean: clean-simulation clean-texts

simulation:
	cd Simulation ; \
	stack test ; \
	stack install

clean-simulation:
	cd Simulation ; \
	stack clean

simulation-results:
	echo "ddd"

pdf:
	$(MAKE) -C TeXts

clean-texts:
	cd TeXts ; \
	make clean
