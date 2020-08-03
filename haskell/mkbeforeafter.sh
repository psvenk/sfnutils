#!/bin/sh

cat >before.lhs <<'EOF'
\documentclass{article}
%include polycode.fmt
\usepackage{url}
\begin{document}
\begin{code}
EOF

cat >after.lhs <<'EOF'
\end{code}
\end{document}
EOF
