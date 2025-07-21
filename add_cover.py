#!/usr/bin/env python3
"""
Add cover image to existing PDF
"""

import sys
import subprocess
import os
from pathlib import Path

def add_cover_to_pdf(pdf_path, cover_path='add-cover.png', output_path=None):
    """Add cover image to PDF using LaTeX"""
    
    if not os.path.exists(pdf_path):
        print(f"❌ PDF not found: {pdf_path}")
        return False
        
    if not os.path.exists(cover_path):
        print(f"❌ Cover image not found: {cover_path}")
        return False
    
    if output_path is None:
        # Add _with_cover suffix
        base = os.path.splitext(pdf_path)[0]
        output_path = f"{base}_with_cover.pdf"
    
    # Create LaTeX document that includes cover and existing PDF
    latex_content = r'''
\documentclass{article}
\usepackage[paperwidth=210mm,paperheight=297mm,margin=0mm]{geometry}
\usepackage{graphicx}
\usepackage{pdfpages}
\pagestyle{empty}

\begin{document}
% Cover page
\includegraphics[width=\paperwidth,height=\paperheight]{''' + cover_path + r'''}
\newpage

% Include the original PDF
\includepdf[pages=-]{''' + pdf_path + r'''}
\end{document}
'''
    
    # Write LaTeX file
    tex_file = 'temp_cover.tex'
    with open(tex_file, 'w') as f:
        f.write(latex_content)
    
    try:
        # Compile LaTeX
        print("📄 Adding cover to PDF...")
        result = subprocess.run(['pdflatex', '-interaction=nonstopmode', tex_file], 
                              capture_output=True, text=True)
        
        if result.returncode == 0:
            # Move result to output path
            os.rename('temp_cover.pdf', output_path)
            print(f"✅ PDF with cover created: {output_path}")
            
            # Clean up temporary files
            for ext in ['.tex', '.log', '.aux']:
                if os.path.exists(f'temp_cover{ext}'):
                    os.remove(f'temp_cover{ext}')
            
            return True
        else:
            print(f"❌ Failed to add cover: {result.stderr}")
            return False
            
    except Exception as e:
        print(f"❌ Error: {e}")
        return False

def main():
    if len(sys.argv) < 2:
        print("Usage: python add_cover.py <pdf_file> [cover_image] [output_file]")
        sys.exit(1)
    
    pdf_file = sys.argv[1]
    cover_image = sys.argv[2] if len(sys.argv) > 2 else 'add-cover.png'
    output_file = sys.argv[3] if len(sys.argv) > 3 else None
    
    add_cover_to_pdf(pdf_file, cover_image, output_file)

if __name__ == '__main__':
    main()