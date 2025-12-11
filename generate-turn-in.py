from pathlib import Path
import shutil

day_no = input('Enter day number: ')

day_dir = Path(f"day{day_no.zfill(2)}")
turnin_dir = "turn-in" / day_dir
turnin_dir.mkdir(parents=True, exist_ok=True)

try:
    for dir in day_dir.glob("ex*/"):
        turnin_ex_dir = turnin_dir / dir.name
        turnin_ex_dir.mkdir(exist_ok=True)
        for file in (dir / "lib").glob("*.ml"):
            shutil.copy(file, turnin_ex_dir / file.name)
    print('Done.')

        
except e:
    print(f"Failed to extract turn-in files for day {day_no}: {e}")
