# mfd_hab_classification

Repo for the classification of the MFD Ontology habitats in the [Microflora Danica](https://github.com/cmc-aau/mfd_wiki/wiki) project and reproduce the results from "Section IV: Convergence of supervised and unsupervised habitat descriptors" of the [MFD manuscripts](https://www.biorxiv.org/content/10.1101/2024.06.27.600767v1).

Please download the input data from the [Zenodo repo](https://zenodo.org/records/12605769) in the `/data` folder and amend the `/config/hab_class.yaml` file as indicated.

The script is meant to run with a SLURM system and requires mamba and conda but installs all the other required packages via mamba. The precise partition names or the resource requirmeent might need to be adjusted in `config/config.yaml` and in the ".rule" files in `scripts/scripts_python/rules/`.
It will aslo try to detect automatically the location where it is. If this fails please amend the file `scripts/scripts_bash/hab_class.sh` as indicated there.


The results from the paper can be reproduced by running:
```
bash scripts/scripts_bash/hab_class.sh
```

The results will be collected in the `/results` folder.

