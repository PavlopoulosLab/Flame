from sys import argv
import json


def json_extract(obj, key):
    """Recursively fetch values from nested JSON."""
    arr = []

    def extract(obj, arr, key):
        """Recursively search for values of key in JSON tree."""
        if isinstance(obj, dict):
            for k, v in obj.items():
                if isinstance(v, (dict, list)):
                    extract(v, arr, key)
                elif k == key:
                    arr.append(v)
        elif isinstance(obj, list):
            for item in obj:
                extract(item, arr, key)
        return arr

    values = extract(obj, arr, key)
    return values


if __name__ == "__main__":
    script, kegg_json, taxid_names = argv
    f = open(kegg_json, "r")
    kg = json.loads(f.read())
    f.close()

    f = open(taxid_names, "r")
    taxids = [i.rstrip().split("\t") for i in f]
    f.close()

    vals = json_extract(kg, "name")

    for i in taxids:
        name = i[1]
        taxon = i[0]
        for v in vals:
            if name in v and "TAX" not in v:
                kegg = v.split()[0]
                print("\t".join([taxon, name, kegg]))
