#!/usr/bin/env ruby

declarations_to_replace = [
  /CHARACTER\*\s*92\s+FILEX_P/,
  /CHARACTER\*\s*80\s+PATHEX/,
  /CHARACTER\*\s*80\s+PATHSL,\s+PATHWT,\s+PATHCR,\s+PATHGE,\s+PATHPE,\s+PATHEC/,
  /CHARACTER\*\s*80\s+PATHWTC,\s+PATHWTG,\s+PATHWTW/,
  /CHARACTER\*\s*80\s+PATHGE/,
  /CHARACTER\*\s*80\s+CHARTEST/,
  /CHARACTER\*\s*80\s+PATHEC/,
  /CHARACTER\*\s*80\s+LINE/,
  /CHARACTER\*\s*80\s+PATHWT/,
  /CHARACTER\*\s*80\s+PATHCR/,
  /CHARACTER\*\s*80\s+PATHWT,\s+LINE/,
  /CHARACTER\*\s*80\s+PATHSL/,
  /CHARACTER\*\s*80\s+PATHGE/,
  /CHARACTER\*\s*80\s+PATHWT,\s+CHARTEST/,
  /CHARACTER\*\s*80\s+CHARTEST,\s+PATHEX/,
  /CHARACTER\*\s*80\s+PATHSD/,
  /CHARACTER\*\s*80\s+SPEpath/,
  /CHARACTER\*\s*80\s+PATHSR/,
  /CHARACTER\*\s*80\s+PATHER/,
  /CHARACTER\*\s*80\s+CHAR/,
  /CHARACTER\*\s*80\s+C80/,
  /CHARACTER\*\s*80\s+CHAR,\s+PATHCR,\s+PATHEC/,
  /CHARACTER\*\s*80\s+PATHCR,\s+CHAR,\s+PATHEC/,
  /CHARACTER\*\s*80\s+PATHCR,\s+CHAR/,
  /CHARACTER\*\s*80\s+TL0801/,
  /CHARACTER\*\s*80\s+FILECC/,
  /CHARACTER\*\s*80\s+PATHPE/,
  /CHARACTER\*\s*80\s+PATHPE,\s+PATHEX/,
  /CHARACTER\*\s*80\s+CHARTEST,\s+PATHSD/,
  /CHARACTER\*92\s+FILECO2/,
  /CHARACTER\*78\s+MSG\(4\)/,
  /CHARACTER\*12\s+SOMFILE/,
  /CHARACTER\*92\s+SOMPF/,
  /CHARACTER\*\s*80\s+PATHWTC,\s+PATHWTG,\s+PATHWTW,\s+WPath/,
  /CHARACTER\*\s*120\s+INPUTX/,
  /CHARACTER\*\s*120\s+INPUTX,\s+FILECTL/,
  /CHARACTER\*\s*120\s+FILECTL/,
  /CHARACTER\*\s*120\s+PATHX/,
  /CHARACTER\*\s*120\s+DATAX/,
  /CHARACTER\*\s*120\s+LINE/,  
  /CHARACTER\*\s*92\s+FILEWW/,
  /CHARACTER\*\s*92\s+FileW/,
  /CHARACTER\*\s*120\s+WTHSTR/
]

# Iterate over all files in the directory with the specified extensions
Dir['./**/*.{for,f90,f,blk}'].each do |file|
  # Read the file content
  content = File.read(file, :encoding => 'ASCII-8BIT')

  declarations_to_replace.each do |regexp|
    content.gsub!(regexp) { |match| match.gsub(/CHARACTER\*\s*(\d+)/, 'CHARACTER*256') }
  end
  content.gsub!('\'(A80)\'', '\'(A256)\'')
  content.gsub!('(1:80)', '(1:256)')
  content.gsub!('WTHSTR(1:120)', 'WTHSTR(1:256)')

  content.gsub!(/CHARACTER\s*\(LEN=80\)/, 'CHARACTER (LEN=256)')
  content.gsub!(/CHARACTER\s*\(len=80\)/, 'CHARACTER (len=256)')
  content.gsub!(/CHARACTER\s*\(LEN=120\)/, 'CHARACTER (LEN=256)')
  content.gsub!('(PATHL .LT. 80)', '(PATHL .LT. 256)')
  content.gsub!('(PATHL+1),80', '(PATHL+1),256')
  content.gsub!('CHARACTER(len=92)  SOILNF', 'CHARACTER(len=256)  SOILNF')
  content.gsub!('CHARACTER(len=12)  FILESS, NAMEF', 'CHARACTER(len=256)  FILESS, NAMEF')

  content.gsub!('CHARACTER*120 DATAX, PATHX', 'CHARACTER*256 DATAX, PATHX')
  content.gsub!('CHARACTER*200 FILESPE, TEXTLINE', 'CHARACTER*256 FILESPE, TEXTLINE')
  content.gsub!('CHARACTER*120 FILECTL, WTHSTR', 'CHARACTER*256 FILECTL, WTHSTR')
  content.gsub!('CHARACTER*120 WTHSTR, FILECTL', 'CHARACTER*256 WTHSTR, FILECTL')

  # Write the modified content back to the file
  File.write(file, content, :encoding => 'ASCII-8BIT')
end 