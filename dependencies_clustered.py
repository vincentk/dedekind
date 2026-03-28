#!/usr/bin/env python3
import os, re, glob

def generate_clustered_graph():
    # Target your specific module directory
    files = glob.glob("src/main/modules/**/*.cppm", recursive=True)
    
    # modules[base_name] = set(partitions)
    modules = {}
    edges = set()

    # Regex for 'export module name : partition;' and 'import name : partition;'
    mod_regex = re.compile(r'^\s*export\s+module\s+([^:; \n]+)(?:\s*:\s*([^; \n]+))?')
    imp_regex = re.compile(r'^\s*import\s+([^:; \n]+)?(?:\s*:\s*([^; \n]+))?')

    for file_path in files:
        with open(file_path, 'r') as f:
            current_base, current_part = None, None
            for line in f:
                # 1. Identify current module/partition
                m = mod_regex.search(line)
                if m:
                    current_base, current_part = m.groups()
                    modules.setdefault(current_base, set()).add(current_part or "primary")
                
                # 2. Identify imports
                i = imp_regex.search(line)
                if i:
                    imp_base, imp_part = i.groups()
                    current_full = f"{current_base}:{current_part}" if current_part else current_base
                    
                    if not imp_base and imp_part: # Internal: import :logic;
                        target = f"{current_base}:{imp_part}"
                        edges.add(f'  "{target}" -> "{current_full}";')
                    elif imp_base: # External: import other:part;
                        target = f"{imp_base}:{imp_part}" if imp_part else imp_base
                        edges.add(f'  "{target}" -> "{current_full}";')

    output_path = "docs/paper/figures/dedekind_modules.dot"
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    with open(output_path, "w") as out:
        out.write("digraph G {\n")
        out.write("  compound=true; rankdir=LR; splines=ortho;\n")
        out.write("  node [shape=box, fontname=\"Helvetica\", style=filled, fillcolor=\"#f9f9f9\"];\n\n")

        # Create clusters for each module
        for base, parts in modules.items():
            # Cluster name must start with 'cluster_' to be drawn with a box
            cluster_id = base.replace(":", "_").replace(".", "_")
            out.write(f'  subgraph cluster_{cluster_id} {{\n')
            out.write(f'    label = "{base}";\n')
            out.write(f'    style = dashed; color = gray; fontname=\"Helvetica-Bold\";\n')
            
            for p in sorted(parts):
                node_id = f"{base}:{p}" if p != "primary" else base
                # Label shows only the partition name for brevity inside the cluster
                label = p if p != "primary" else "main"
                out.write(f'    "{node_id}" [label="{label}"];\n')
            out.write("  }\n\n")

        out.write("\n".join(sorted(edges)) + "\n}")

    print(f"Generated clustered graph for {len(modules)} primary modules.")

if __name__ == '__main__':
    generate_clustered_graph()

