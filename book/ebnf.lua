function CodeBlock(el)
  if el.classes:includes("ebnf") then
    return pandoc.RawBlock(
      "latex",
      "\\begin{ebnf}\n" .. el.text .. "\n\\end{ebnf}"
    )
  end
end
