import xlsxwriter

workbook = xlsxwriter.Workbook('sine.xlsx')

worksheet = workbook.add_worksheet('chart')

data = {}
for i in range(0, 181):
    data.update({f'A{i}': i})

n = 0

for x in range(0, 181):
    worksheet.write(str(list(data.keys())[n]), str(list(data.values())[n]))
    worksheet.write(f'B{n}', f'=sin(radians(A{n}))')
    n = n + 1

chart = workbook.add_chart({'type': 'line'})

chart.add_series({
    'name': 'angle',
    'values': '=chart!$A$1:$A$100',
})
chart.add_series({
    'name': 'sine',
    'values': '=chart!$B$1:$B$100',
})

worksheet.insert_chart('D3', chart)
workbook.close()
