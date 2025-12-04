import random

def get_monthly_precip(month):
    # Monthly averages in inches, converted to mm (1 inch = 25.4 mm)
    monthly_avg_in = {
        1: 2.12, 2: 2.11, 3: 2.98, 4: 3.80, 5: 4.62, 6: 4.20,
        7: 4.05, 8: 3.60, 9: 3.24, 10: 3.26, 11: 3.47, 12: 2.74
    }
    return monthly_avg_in.get(month, 3.0) * 25.4

def generate_rain(month):
    # Simple weather generator: 25% chance of rain
    if random.random() < 0.25:
        # If it rains, amount is roughly 4x the daily average to balance the 25% prob
        # Daily avg = Monthly / 30
        daily_avg = get_monthly_precip(month) / 30.0
        amount = daily_avg * 4.0 * random.uniform(0.5, 1.5) # Add variability
        return round(amount, 1)
    return 0.0

input_file = 'UFGA0101.WTH'
output_lines = []

with open(input_file, 'r') as f:
    for line in f:
        if line.startswith('@') or line.startswith('*'):
            output_lines.append(line)
            continue
            
        try:
            # Parse date to get month
            # Format: 01001 (YYDDD)
            year_doy = line[:5]
            doy = int(year_doy[2:])
            
            # Rough month estimation from DOY
            if doy <= 31: month = 1
            elif doy <= 59: month = 2
            elif doy <= 90: month = 3
            elif doy <= 120: month = 4
            elif doy <= 151: month = 5
            elif doy <= 181: month = 6
            elif doy <= 212: month = 7
            elif doy <= 243: month = 8
            elif doy <= 273: month = 9
            elif doy <= 304: month = 10
            elif doy <= 334: month = 11
            else: month = 12
            
            rain = generate_rain(month)
            
            # Preserve fixed width formatting
            # DATE  SRAD  TMAX  TMIN  RAIN
            # 01001 12.5  20.0  10.0  0.0
            date = line[:5]
            srad = line[6:11]
            tmax = line[12:17]
            tmin = line[18:23]
            
            # Format rain as F5.1
            new_line = f"{date} {srad} {tmax} {tmin} {rain:5.1f}\n"
            output_lines.append(new_line)
            
        except ValueError:
            output_lines.append(line)

with open(input_file, 'w') as f:
    f.writelines(output_lines)

print("Updated UFGA0101.WTH with Illinois precipitation data.")
