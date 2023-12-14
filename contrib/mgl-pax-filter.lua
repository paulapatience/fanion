--* mgl-pax-filter.lua — Pandoc filter for MGL-PAX's Markdown output
--
-- SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
-- SPDX-License-Identifier: MIT

local stringify = pandoc.utils.stringify

local title = nil
local first_inpackage = true

if FORMAT:match 'latex' then
  function Header(el)
    if el.level == 1 then
      title = stringify(el.content)
      return {}
    elseif el.level == 6 and stringify(el.content):match '^%[in package' then
      local skip = ''
      if first_inpackage then
        skip = '\\vspace{-8pt}'
        first_inpackage = false
      else
        skip = '\\bigskip'
      end
      return pandoc.Para{
        pandoc.RawInline('latex', '\\hbox{\\sffamily\\small'),
        stringify(el.content),
        pandoc.RawInline('latex', '}' .. skip)
      }
    else
      -- Instead of --shift-heading-level=-1, in order to detect
      -- MGL-PAX's title above.
      el.level = el.level - 1
      return el
    end
  end

  function Link(el)
    if el.target:match '^#' then
      table.insert(el.content, 1, pandoc.RawInline('latex', '\\hyperref[' .. el.target:sub(2) .. ']{'))
      table.insert(el.content, pandoc.RawInline('latex', '}'))
      return el.content
    end
  end

  function Pandoc(doc)
    table.remove(doc.blocks, 1) -- The link before the title.
    local anchor = nil
    local anchor_filter = {
      RawInline = function (el)
        if el.format == 'html' and el.text:match '^<a id="x%-28' then
          anchor = el.text:gsub('^<a id="([^"]*)">$', "%1")
          return {}
        end
      end
    }
    local i = 1
    while i < #doc.blocks do
      pandoc.walk_block(pandoc.Div(doc.blocks[i]), anchor_filter)
      i = i + 1
      if anchor ~= nil then
        if doc.blocks[i].t == 'Header' then
          doc.blocks[i-1] = doc.blocks[i]
          doc.blocks[i] = pandoc.RawInline('latex', '\\label{' .. anchor .. '}')
        elseif doc.blocks[i].t == 'BulletList' then
          table.remove(doc.blocks, i-1)
          table.insert(doc.blocks[i-1].content[1][1].content, 1,
                       pandoc.RawInline('latex', '\\phantomsection\\label{' .. anchor .. '}'))
        end
        anchor = nil
      end
    end
    return doc
  end
end

function Meta(m)
  if title ~= nil then
    m.title = title
  end
  if m.date == nil then
    m.date = os.date("%e %B %Y")
  end
  return m
end
