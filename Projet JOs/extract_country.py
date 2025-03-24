def extract_country_data(csv_path):
    countries = []
    populations = []

    with open(csv_path, 'r') as file:
        for line in file:
            fields = line.strip().split(',')
            if len(fields) >= 3:
                try:
                    # Verify it's a data row by checking if first column is a number
                    int(fields[0])
                    country = fields[1]
                    population = fields[2].replace(',', '')

                    countries.append(country)
                    populations.append(population)
                except ValueError:
                    # Skip header or invalid rows
                    continue

    # Print in R data.frame format
    print("# Population data manually entered")
    print("population_data <- data.frame(")
    print("  Pays_Pop = c(")
    for i, country in enumerate(countries):
        if i < len(countries) - 1:
            print(f'    "{country}",')
        else:
            print(f'    "{country}"')
    print("  ),")
    print("  Population = c(")
    for i, pop in enumerate(populations):
        if i < len(populations) - 1:
            print(f"    {pop},")
        else:
            print(f"    {pop}")
    print("  )")
    print(")")

# Usage
extract_country_data("WorldPopulation2023.csv")