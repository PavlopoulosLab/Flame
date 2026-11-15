export interface Publication {
  authors: string
  year: number
  title: string
  journal: string
  details: string
  doi: string
  pmid: string
  pmcid: string
  bibtexKey: string
}

export const PUBLICATIONS: Publication[] = [
  {
    authors: 'Karatzas E., Baltoumas F.A., Aplakidou E., Kontou P.I., Stathopoulos P., Stefanis L., Bagos P.G., Pavlopoulos G.A.',
    year: 2023,
    title: 'Flame (v2.0): advanced integration and interpretation of functional enrichment results from multiple sources',
    journal: 'Bioinformatics',
    details: '39(8):btad490',
    doi: '10.1093/bioinformatics/btad490',
    pmid: '37540207',
    pmcid: 'PMC10423032',
    bibtexKey: 'karatzas2023flame',
  },
  {
    authors: 'Thanati F., Karatzas E., Baltoumas F.A., Stravopodis D.J., Eliopoulos A.G., Pavlopoulos G.A.',
    year: 2021,
    title: 'FLAME: a web tool for functional and literature enrichment analysis of multiple gene lists',
    journal: 'Biology',
    details: '10(7):665',
    doi: '10.3390/biology10070665',
    pmid: '34356520',
    pmcid: 'PMC8301326',
    bibtexKey: 'thanati2021flame',
  },
]
