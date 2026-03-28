#!/usr/bin/env python3
import os
import re
import glob

def generate_source_graph():
    # Find all potential module files
    # Adjust the extension list if you use others (e.g., .ixx)
    extensions = ['*.cppm', '*.ixx', '*.cpp', '*.h']
    files = []
    for ext in extensions:
        files.extend(glob.glob("src/main/modules/**/*.cppm", recursive=True))

    nodes = set()
    edges = set()

    # Regex to find: export module name; OR export module name : partition;
    # Regex to find: import name; OR import : partition;
    mod_regex = re.compile(r'^\s*export\s+module\s+([^:; \n]+)(?:\s*:\s*([^; \n]+))?')
    imp_regex = re.compile(r'^\s*import\s+([^:; \n]+)?(?:\s*:\s*([^; \n]+))?')

    for file_path in files:
        with open(file_path, 'r') as f:
            current_mod = None
            for line in f:
                # 1. Identify what this file provides
                mod_match = mod_regex.search(line)
                if mod_match:
                    name, part = mod_match.groups()
                    current_mod = f"{name}:{part}" if part else name
                    nodes.add(current_mod)
                    continue
                
                # 2. Identify what this file imports
                imp_match = imp_regex.search(line)
                if imp_match:
                    name, part = imp_match.groups()
                    # Handle internal partition imports like 'import :logic;'
                    if not name and part and current_mod:
                        base = current_mod.split(':')[0]
                        target = f"{base}:{part}"
                        edges.add(f'  "{target}" -> "{current_mod}";')
                    # Handle full module imports
                    elif name:
                        target = f"{name}:{part}" if part else name
                        if current_mod:
                            edges.add(f'  "{target}" -> "{current_mod}";')

    # Output to the specified directory
    output_path = "docs/paper/figures/dedekind_modules.dot"
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    with open(output_path, "w") as out:
        out.write("digraph G {\n  rankdir=LR;\n")
        out.write("  node [shape=box, fontname=\"Arial\", style=filled, fillcolor=\"#f9f9f9\"];\n")
        for node in sorted(nodes): out.write(f'  "{node}";\n')
        out.write("\n" + "\n".join(sorted(edges)) + "\n}")

    print(f"Parsed {len(files)} files. Found {len(nodes)} modules and {len(edges)} links.")

if __name__ == '__main__':
    generate_source_graph()

