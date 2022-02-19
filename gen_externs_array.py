import os

import os
o = []
for root, dirs, files in os.walk("staging/output", topdown=False):
   for name in files:
      if name.split('.')[-1] == 'cbor':
        o.append(os.path.join(root, name).replace('staging/output/', ''))

with open('src/Externs.hs', 'w') as externs:
  externs.write("""module Externs(externFileList) where

externFileList :: [String]
externFileList = [""")
  externs.write(','.join(['"%s"' % x for x in o]))
  externs.write(']')
