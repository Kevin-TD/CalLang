# CalLang execution

# Insert source of .cal file as the first argument, and it will be 'compiled'
# and ran

import sys
import os

cl_filename = sys.argv[1]

os.system(f"""
          cd compilation;
          dune exec ./cl_compilation.exe ../{cl_filename};
          python3 ../runtime_exec/cl_runtime.py ../{cl_filename}.json ../credentials.json;
          rm ../{cl_filename}.json
          """)  