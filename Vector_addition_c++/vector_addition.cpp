#include <iostream>
#include <new> 
#include <limits> 

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " N_OPS N_ENTRIES\n";
        return 1;
    }

    int n_ops, n_entries;
    try {
        n_ops = std::stoi(argv[1]);
        n_entries = std::stoi(argv[2]);
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << ". N_OPS and N_ENTRIES must be integers.\n";
        return 1;
    }

    if (n_ops <= 0 || n_entries <= 0) {
        std::cerr << "N_OPS and N_ENTRIES must be positive integers.\n";
        return 1;
    }

    for (int op = 0; op < n_ops; ++op) {
        int* vector1 = nullptr;
        int* vector2 = nullptr;
        int* sum_vector = nullptr;

        try {
            vector1 = new int[n_entries];
            vector2 = new int[n_entries];
            sum_vector = new int[n_entries];
        } catch (const std::bad_alloc& e) {
            std::cerr << "Memory allocation failed: " << e.what() << '\n';
            // Cleaning up already allocated memory before exiting
            delete[] vector1;
            delete[] vector2;
           
            return 1;
        }

        
        for (int i = 0; i < n_entries; ++i) {
            if (!(std::cin >> vector1[i])) {
                std::cerr << "Failed to read input for vector 1, entry " << i << ".\n";
                delete[] vector1;
                delete[] vector2;
                delete[] sum_vector;
                return 1;
            }
        }

        for (int i = 0; i < n_entries; ++i) {
            if (!(std::cin >> vector2[i])) {
                std::cerr << "Failed to read input for vector 2, entry " << i << ".\n";
                delete[] vector1;
                delete[] vector2;
                delete[] sum_vector;
                return 1;
            }
        }

        // Adding 2 vectors
        for (int i = 0; i < n_entries; ++i) {
            sum_vector[i] = vector1[i] + vector2[i];
        }

        //Sum vector
        for (int i = 0; i < n_entries; ++i) {
            std::cout << sum_vector[i] << ' ';
        }
        std::cout << "\n\n";

        // Free the allocated memory
        delete[] vector1;
        delete[] vector2;
        delete[] sum_vector;
    }

    return 0;
}
