FrozenBeagle: simulation simulation-results pdf

clean: clean-simulation clean-texts

simulation:
	cd Simulation/Lib ; \
	stack test ; \
	cd .. ; \
	stack install

clean-simulation:
	cd Simulation ; \
	stack clean ; \
	cd ..

simulation-results:
	echo "Running Simulation... sorta."

pdf:
	$(MAKE) -C TeXts

clean-texts:
	cd TeXts ; \
	make clean
