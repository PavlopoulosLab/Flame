/**
 * Publications of the tools and resources Flame uses (latest article of each,
 * checked against Crossref). Shown on the How to cite page.
 */
export interface Citation { key: string; resource: string; ref: string; doi: string }

export const CITATION_GROUPS: { title: string; items: Citation[] }[] = [
  { title: 'Enrichment tools', items: [
    { key: 'gprofiler', resource: 'g:Profiler', ref: 'Kolberg L et al. (2023) g:Profiler—interoperable web service for functional enrichment analysis and gene identifier mapping (2023 update). Nucleic Acids Res 51:W207–W212', doi: '10.1093/nar/gkad347' },
    { key: 'string', resource: 'STRING', ref: 'Szklarczyk D et al. (2025) The STRING database in 2025: protein networks with directionality of regulation. Nucleic Acids Res 53:D730–D737', doi: '10.1093/nar/gkae1113' },
    { key: 'enrichr', resource: 'Enrichr', ref: 'Xie Z et al. (2021) Gene set knowledge discovery with Enrichr. Curr Protoc 1:e90', doi: '10.1002/cpz1.90' },
    { key: 'webgestalt', resource: 'WebGestalt', ref: 'Elizarraras JM et al. (2024) WebGestalt 2024: faster gene set analysis and new support for metabolomics and multi-omics. Nucleic Acids Res 52:W415–W421', doi: '10.1093/nar/gkae456' },
    { key: 'panther', resource: 'PANTHER', ref: 'Thomas PD et al. (2022) PANTHER: making genome-scale phylogenetics accessible to all. Protein Sci 31:8–22', doi: '10.1002/pro.4218' },
    { key: 'genecodis', resource: 'GeneCodis', ref: 'Garcia-Moreno A et al. (2022) Functional enrichment analysis of regulatory elements. Biomedicines 10:590', doi: '10.3390/biomedicines10030590' },
    { key: 'reactome', resource: 'Reactome', ref: 'Milacic M et al. (2024) The Reactome Pathway Knowledgebase 2024. Nucleic Acids Res 52:D672–D678', doi: '10.1093/nar/gkad1025' },
    { key: 'reactomeas', resource: 'Reactome Analysis Service', ref: 'Fabregat A et al. (2017) Reactome pathway analysis: a high-performance in-memory approach. BMC Bioinformatics 18:142', doi: '10.1186/s12859-017-1559-2' },
    { key: 'clusterprofiler', resource: 'clusterProfiler', ref: 'Xu S et al. (2024) Using clusterProfiler to characterize multiomics data. Nat Protoc 19:3292–3320', doi: '10.1038/s41596-024-01020-z' },
    { key: 'fgsea', resource: 'fgsea', ref: 'Korotkevich G et al. (2016) Fast gene set enrichment analysis. bioRxiv 060012', doi: '10.1101/060012' },
  ] },
  { title: 'Gene sets, pathways and ontologies', items: [
    { key: 'go', resource: 'Gene Ontology', ref: 'Gene Ontology Consortium (2023) The Gene Ontology knowledgebase in 2023. Genetics 224:iyad031', doi: '10.1093/genetics/iyad031' },
    { key: 'kegg', resource: 'KEGG', ref: 'Kanehisa M et al. (2025) KEGG: biological systems database as a model of the real world. Nucleic Acids Res 53:D672–D677', doi: '10.1093/nar/gkae909' },
    { key: 'wikipathways', resource: 'WikiPathways', ref: 'Agrawal A et al. (2024) WikiPathways 2024: next generation pathway database. Nucleic Acids Res 52:D679–D689', doi: '10.1093/nar/gkad960' },
    { key: 'msigdb', resource: 'MSigDB', ref: 'Liberzon A et al. (2015) The Molecular Signatures Database Hallmark gene set collection. Cell Syst 1:417–425', doi: '10.1016/j.cels.2015.12.004' },
    { key: 'msigdbmouse', resource: 'MSigDB (mouse)', ref: 'Castanza AS et al. (2023) Extending support for mouse data in the Molecular Signatures Database (MSigDB). Nat Methods 20:1619–1620', doi: '10.1038/s41592-023-02014-7' },
    { key: 'genesigdb', resource: 'GeneSigDB', ref: 'Culhane AC et al. (2012) GeneSigDB: a manually curated database and resource for analysis of gene expression signatures. Nucleic Acids Res 40:D1060–D1066', doi: '10.1093/nar/gkr901' },
    { key: 'rummagene', resource: 'Rummagene', ref: 'Clarke DJB et al. (2024) Rummagene: massive mining of gene sets from supporting materials of biomedical research publications. Commun Biol 7:482', doi: '10.1038/s42003-024-06177-7' },
  ] },
  { title: 'Regulators, drugs, cell types and activity', items: [
    { key: 'chea3', resource: 'ChEA3', ref: 'Keenan AB et al. (2019) ChEA3: transcription factor enrichment analysis by orthogonal omics integration. Nucleic Acids Res 47:W212–W224', doi: '10.1093/nar/gkz446' },
    { key: 'kea3', resource: 'KEA3', ref: 'Kuleshov MV et al. (2021) KEA3: improved kinase enrichment analysis via data integration. Nucleic Acids Res 49:W304–W316', doi: '10.1093/nar/gkab359' },
    { key: 'lincs', resource: 'LINCS L1000', ref: 'Subramanian A et al. (2017) A next generation Connectivity Map: L1000 platform and the first 1,000,000 profiles. Cell 171:1437–1452', doi: '10.1016/j.cell.2017.10.049' },
    { key: 'panglaodb', resource: 'PanglaoDB', ref: 'Franzén O et al. (2019) PanglaoDB: a web server for exploration of mouse and human single-cell RNA sequencing data. Database 2019:baz046', doi: '10.1093/database/baz046' },
    { key: 'cellmarker', resource: 'CellMarker', ref: 'Hu C et al. (2023) CellMarker 2.0: an updated database of manually curated cell markers in human/mouse and web tools based on scRNA-seq data. Nucleic Acids Res 51:D870–D876', doi: '10.1093/nar/gkac947' },
    { key: 'azimuth', resource: 'Azimuth', ref: 'Hao Y et al. (2021) Integrated analysis of multimodal single-cell data. Cell 184:3573–3587', doi: '10.1016/j.cell.2021.04.048' },
    { key: 'tabula', resource: 'Tabula Sapiens', ref: 'Tabula Sapiens Consortium (2022) The Tabula Sapiens: a multiple-organ, single-cell transcriptomic atlas of humans. Science 376:eabl4896', doi: '10.1126/science.abl4896' },
    { key: 'progeny', resource: 'PROGENy', ref: 'Schubert M et al. (2018) Perturbation-response genes reveal signaling footprints in cancer gene expression. Nat Commun 9:20', doi: '10.1038/s41467-017-02391-6' },
    { key: 'ssgsea', resource: 'ssGSEA', ref: 'Barbie DA et al. (2009) Systematic RNA interference reveals that oncogenic KRAS-driven cancers require TBK1. Nature 462:108–112', doi: '10.1038/nature08460' },
    { key: 'mirdb', resource: 'miRDB', ref: 'Chen Y, Wang X (2020) miRDB: an online database for prediction of functional microRNA targets. Nucleic Acids Res 48:D127–D131', doi: '10.1093/nar/gkz757' },
    { key: 'rra', resource: 'Robust rank aggregation', ref: 'Kolde R et al. (2012) Robust rank aggregation for gene list integration and meta-analysis. Bioinformatics 28:573–580', doi: '10.1093/bioinformatics/btr709' },
  ] },
  { title: 'Diseases, variants and literature', items: [
    { key: 'opentargets', resource: 'Open Targets', ref: 'Buniello A et al. (2025) Open Targets Platform: facilitating therapeutic hypotheses building in drug discovery. Nucleic Acids Res 53:D1467–D1475', doi: '10.1093/nar/gkae1128' },
    { key: 'disgenet', resource: 'DisGeNET', ref: 'Piñero J et al. (2020) The DisGeNET knowledge platform for disease genomics: 2019 update. Nucleic Acids Res 48:D845–D855', doi: '10.1093/nar/gkz1021' },
    { key: 'omim', resource: 'OMIM', ref: 'Amberger JS et al. (2019) OMIM.org: leveraging knowledge across phenotype–gene relationships. Nucleic Acids Res 47:D1038–D1043', doi: '10.1093/nar/gky1151' },
    { key: 'jensen', resource: 'DISEASES', ref: 'Grissa D et al. (2022) Diseases 2.0: a weekly updated database of disease–gene associations from text mining and data integration. Database 2022:baac019', doi: '10.1093/database/baac019' },
    { key: 'gwas', resource: 'GWAS Catalog', ref: 'Cerezo M et al. (2025) The NHGRI-EBI GWAS Catalog: standards for reusability, sustainability and diversity. Nucleic Acids Res 53:D998–D1005', doi: '10.1093/nar/gkae1070' },
    { key: 'clinvar', resource: 'ClinVar', ref: 'Landrum MJ et al. (2025) ClinVar: updates to support classifications of both germline and somatic variants. Nucleic Acids Res 53:D1313–D1321', doi: '10.1093/nar/gkae1090' },
    { key: 'ncbi', resource: 'NCBI (Gene, PubMed, dbSNP, GEO)', ref: 'Sayers EW et al. (2025) Database resources of the National Center for Biotechnology Information in 2025. Nucleic Acids Res 53:D20–D29', doi: '10.1093/nar/gkae979' },
    { key: 'geo', resource: 'GEO', ref: 'Clough E et al. (2024) NCBI GEO: archive for gene expression and epigenomics data sets: 23-year update. Nucleic Acids Res 52:D138–D144', doi: '10.1093/nar/gkad965' },
    { key: 'rummageo', resource: 'RummaGEO', ref: 'Marino GB et al. (2024) RummaGEO: automatic mining of human and mouse gene sets from GEO. Patterns 5:101072', doi: '10.1016/j.patter.2024.101072' },
    { key: 'vep', resource: 'Ensembl VEP', ref: 'McLaren W et al. (2016) The Ensembl Variant Effect Predictor. Genome Biol 17:122', doi: '10.1186/s13059-016-0974-4' },
    { key: 'ensembl', resource: 'Ensembl', ref: 'Dyer SC et al. (2025) Ensembl 2025. Nucleic Acids Res 53:D948–D957', doi: '10.1093/nar/gkae1071' },
    { key: 'gnomad', resource: 'gnomAD', ref: 'Chen S et al. (2024) A genomic mutational constraint map using variation in 76,156 human genomes. Nature 625:92–100', doi: '10.1038/s41586-023-06045-0' },
    { key: 'extract', resource: 'EXTRACT', ref: 'Pafilis E et al. (2016) EXTRACT: interactive extraction of environment metadata and term suggestion for metagenomic sample annotation. Database 2016:baw005', doi: '10.1093/database/baw005' },
  ] },
  { title: 'Network viewers', items: [
    { key: 'arena3d', resource: 'Arena3Dweb', ref: 'Kokoli M et al. (2023) Arena3Dweb: interactive 3D visualization of multilayered networks supporting multiple directional information channels, clustering analysis and application integration. NAR Genom Bioinform 5:lqad053', doi: '10.1093/nargab/lqad053' },
    { key: 'norma', resource: 'NORMA', ref: 'Koutrouli M et al. (2022) NORMA: the network makeup artist — a web tool for network annotation visualization. Genomics Proteomics Bioinformatics 20:578–586', doi: '10.1016/j.gpb.2021.02.005' },
  ] },
]
