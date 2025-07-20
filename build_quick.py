#!/usr/bin/env python3
"""
Quick build script that skips dependency checks
"""

import sys
import os

# Add the current directory to Python path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Import the main builder
from build import BookBuilder

class QuickBookBuilder(BookBuilder):
    def check_dependencies(self):
        """Skip dependency check"""
        print("⚡ Skipping dependency check (quick mode)")
        print("⚠️  Assuming pandoc and lualatex are installed")

if __name__ == '__main__':
    builder = QuickBookBuilder()
    builder.build(['pdf'])