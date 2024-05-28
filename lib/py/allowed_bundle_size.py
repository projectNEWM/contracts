
import csv

if __name__ == "__main__":
    N = 100 * pow(10, 6)

    with open('output.csv', mode='w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['Bundle_Size', 'Number_Of_Bundles'])  # Header row

        for i in range(1, N+1):
            if N % i == 0:
                writer.writerow([i, N // i])
