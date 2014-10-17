INFORMAL_BON_FILES = domain_model/E2EVIV.bon
FORMAL_BON_FILES = 
REQUIREMENTS = requirements/accessibility.bon requirements/assurance.bon requirements/auditing.bon requirements/authentication.bon requirements/certification.bon requirements/e2eviv.bon requirements/evolvability.bon requirements/functional.bon requirements/interoperability.bon requirements/legal.bon requirements/maintenance.bon requirements/non-functional.bon requirements/operational.bon requirements/procedural.bon requirements/reliability.bon requirements/security.bon requirements/system_operational.bon requirements/technical.bon requirements/usability.bon
BON_FILES = $(INFORMAL_BON_FILES) $(FORMAL_BON_FILES) $(REQUIREMENTS)

.PHONY: all bonc

all: bonc e2eviv-iig.dot index.txt index.html html/index.html

bonc:
	bonc -i $(BON_FILES)

e2eviv-icg.dot: $(BON_FILES)
	bonc -g ICG $(BON_FILES) > e2eviv-icg.dot

e2eviv-iig.dot: $(BON_FILES)
	bonc -p IIG -po e2eviv-iig.dot $(BON_FILES)

index.txt: $(BON_FILES)
	bonc -po index.txt -p TXT $(BON_FILES)

index.html: $(BON_FILES)
	bonc -po index.html -p HTML $(BON_FILES)

html/index.html: $(BON_FILES)
	bonc -p NEWHTML -po html $(BON_FILES)
