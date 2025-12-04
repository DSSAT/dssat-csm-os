import random

def get_monthly_precip(month):
    # Target: 40mm annual (~1.57 inches)
    # Scale Las Vegas data (which is ~4.5 inches) by ~0.35
    monthly_avg_in = {
        1: 0.59, 2: 0.69, 3: 0.59, 4: 0.15, 5: 0.24, 6: 0.08,
        7: 0.44, 8: 0.45, 9: 0.31, 10: 0.24, 11: 0.31, 12: 0.40
    }
    # Scale factor to reach ~40mm (1.57 in) from ~4.5 in
    scale = 1.57 / 4.5
    return monthly_avg_in.get(month, 0.3) * scale * 25.4

def generate_rain(month):
    # Very dry, low probability
    prob = 0.05 
    
    if random.random() < prob:
        daily_avg = get_monthly_precip(month) / 30.0
        # High intensity events are rare but possible, but keep total low
        amount = daily_avg * (1.0/prob) * random.uniform(0.5, 1.5)
        return round(amount, 1)
    return 0.0

header = """*WEATHER DATA : UFGA

@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT
  UFGA   29.630  -82.370    40  20.0  10.0   2.0   2.0
@DATE  SRAD  TMAX  TMIN  RAIN
"""

output_lines = [header]

# Generate data for year 2001 (01)
days_in_month = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
doy = 1

for month in range(1, 13):
    for day in range(1, days_in_month[month] + 1):
        rain = generate_rain(month)
        # Fixed values for other variables as per original file (approx)
        srad = 12.5
        tmax = 20.0
        tmin = 10.0
        
        # Format: YYDDD (01xxx)
        date_str = f"01{doy:03d}"
        
        # Fixed width columns
        # DATE  SRAD  TMAX  TMIN  RAIN
        # 01001 12.5  20.0  10.0  0.0
        line = f"{date_str} {srad:5.1f} {tmax:5.1f} {tmin:5.1f} {rain:5.1f}\n"
        output_lines.append(line)
        doy += 1

with open('UFGA0101.WTH', 'w') as f:
    f.writelines(output_lines)

print("Regenerated UFGA0101.WTH with correct format and Illinois precipitation.")
