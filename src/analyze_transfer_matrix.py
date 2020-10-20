import os
import numpy as np
from scipy import *
from scipy.sparse import *
import time

def read_transfer_matrix(matrix_file_name):
    assert(os.path.exists(matrix_file_name))
    row_indices = []
    col_indices = []
    data = []
    reader = open(matrix_file_name, "r")
    for (index, string) in enumerate(reader):
        a = string.strip("\n").split(",")
        row_indices.append(int(a[0]))
        col_indices.append(int(a[1]))
        data.append(float(a[2]))
    reader.close()
    dimension = max(max(row_indices), max(col_indices)) + 1
    transfer_matrix = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, dimension))
    return transfer_matrix

def sparse_vector_l2_norm(sparse_vector):
    vector = np.asarray(sparse_vector.todense())[:, 0]
    return np.linalg.norm(vector)
    #return np.sqrt(sum(sparse_vector.transpose() * sparse_vector))

def sparse_vector_l1_norm(sparse_vector):
    vector = np.asarray(sparse_vector.todense())[:, 0]
    return np.linalg.norm(vector, 1)

def sparse_vector_norm(sparse_vector, order = 2):
    vector = np.asarray(sparse_vector.todense())[:, 0]
    return np.linalg.norm(vector, order)

def get_google_vector(alpha, dimension):
    assert(alpha <= 1 and alpha >= 0)
    row_indices = []
    col_indices = [0] * dimension
    data = []
    for i in range(dimension):
        row_indices.append(i)
        data.append((1 - alpha)/float(dimension))
    google_vector = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, 1))
    return google_vector

def power_iteration(sparse_matrix, alpha):
    import sys
    start_time = time.time()
    assert(alpha >= 0 and alpha <= 1)
    
    order = 1
    if (alpha != 1 and order != 1):
        print "Error. If you use damping factor, you must use L1 norm to normalize your intermediate state vectors during power iteration process. "
        sys.exit(-1)

    row, col = sparse_matrix.shape
    assert(row == col)
    dimension = row

    row_indices = []
    col_indices = [0] * dimension
    data = []
    for i in range(dimension):
        row_indices.append(i)
        data.append(np.random.uniform())

    initial = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, 1))
    initial = initial/sparse_vector_norm(initial, order)
    updated = initial

    google_vector = None
    if (alpha < 1):
        google_vector = get_google_vector(alpha, dimension)

    iteration_max = 1200
    counter = 0
    eps = 1.0e-15
    if (alpha == 1):
        while True:
            counter += 1
            updated = sparse_matrix * initial
            norm = sparse_vector_norm(updated, order)
            assert(norm > 0)
            updated = updated / norm
            diff = updated - initial
            error = sparse_vector_norm(diff)
            print "Counter = " + str(counter) + ", error = " + str(error) + ", total = " + str(iteration_max)
            initial = updated
            if (error < eps or counter > iteration_max):
                break
    else:
        while True:
            counter += 1
            updated = sparse_matrix * initial * alpha + google_vector
            norm = sparse_vector_norm(updated, order)
            assert(norm > 0)
            updated = updated/norm
            diff = updated - initial
            error = sparse_vector_norm(diff)
            print "Counter = " + str(counter) + ", error = " + str(error) + ", total = " + str(iteration_max)
            initial = updated
            if (error < eps or counter > iteration_max):
                break

    print "Total Counter = " + str(counter) + ", error = " + str(error)
    leading_eigenvalue = (sum(initial.transpose() * sparse_matrix * initial) * alpha + (1.0 - alpha)/float(dimension)) / sum(initial.transpose() * initial)
    end_time = time.time()
    print "Total time used in power iteration: " + str(end_time - start_time)
    return leading_eigenvalue, initial

def power_iteration_with_personalization(sparse_matrix, alpha, personalization_vector):
    import sys
    start_time = time.time()
    assert(alpha >= 0 and alpha <= 1)
    
    order = 1
    if (alpha != 1 and order != 1):
        print "Error. If you use damping factor, you must use L1 norm to normalize your intermediate state vectors during power iteration process. "
        sys.exit(-1)

    row, col = sparse_matrix.shape
    assert(row == col)
    dimension = row
    row, col = personalization_vector.shape
    assert(col == 1 and row == dimension)

    row_indices = []
    col_indices = [0] * dimension
    data = []
    for i in range(dimension):
        row_indices.append(i)
        data.append(np.random.uniform())

    initial = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, 1))
    initial = initial/sparse_vector_norm(initial, order)
    updated = initial

    google_vector = None
    if (alpha < 1):
        google_vector = (1 - alpha) * personalization_vector 

    iteration_max = 1200
    counter = 0
    eps = 1.0e-15
    if (alpha == 1):
        while True:
            counter += 1
            updated = sparse_matrix * initial
            norm = sparse_vector_norm(updated, order)
            assert(norm > 0)
            updated = updated / norm
            diff = updated - initial
            error = sparse_vector_norm(diff)
            print "Counter = " + str(counter) + ", error = " + str(error) + ", total = " + str(iteration_max)
            initial = updated
            if (error < eps or counter > iteration_max):
                break
    else:
        while True:
            counter += 1
            updated = sparse_matrix * initial * alpha + google_vector
            norm = sparse_vector_norm(updated, order)
            assert(norm > 0)
            updated = updated/norm
            diff = updated - initial
            error = sparse_vector_norm(diff)
            print "Counter = " + str(counter) + ", error = " + str(error) + ", total = " + str(iteration_max)
            initial = updated
            if (error < eps or counter > iteration_max):
                break

    print "Total Counter = " + str(counter) + ", error = " + str(error)
    leading_eigenvalue = (sum(initial.transpose() * sparse_matrix * initial) * alpha + (1.0 - alpha)/float(dimension)) / sum(initial.transpose() * initial)
    end_time = time.time()
    print "Total time used in power iteration: " + str(end_time - start_time)
    return leading_eigenvalue, initial

def generate_sparse_matrix(dimension, filling_factor):
    assert(filling_factor > 0 and filling_factor <= 1)
    assert(dimension >= 1)
    row_indices = []
    col_indices = []
    data = []
    for i in range(dimension * dimension):
        if (np.random.uniform() < filling_factor):
            row_indices.append(np.random.randint(0, dimension))
            col_indices.append(np.random.randint(0, dimension))
            data.append(np.random.uniform())
    sparse_matrix = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, dimension))
    return sparse_matrix

def sparse_vector_to_string(sparse_vector):
    v = np.asarray(sparse_vector.todense())[:, 0]
    return ", ".join(map(str, v))

def get_leading_from_numpy(sparse_matrix, alpha):
    start_time = time.time()

    A = None
    if (alpha == 1):
        A = sparse_matrix.todense()
    else:
        A = sparse_matrix.todense()
        (row, col) = A.shape
        assert(row == col)
        dimension = row
        A = alpha * A + (1.0 - alpha)/float(dimension) * np.ones((dimension, dimension))
    v, U = np.linalg.eig(A)
    #print "All eigenvalues: " + ", ".join(map(lambda ele: str(abs(ele)), v))
    max_index = 0
    max_eigenvalue = abs(v[0])
    for i in range(1, len(v)):
        if (abs(v[i]) > max_eigenvalue):
            max_index = i
            max_eigenvalue = v[i]
    #print "Leading eigenvalue from numpy : " + str(max_eigenvalue)
    matrix = np.asarray(U)
    max_eigenvector = matrix[:, max_index]
    #print "Leading eigenvector from numpy: " + ", ".join(map(str, matrix[:, max_index]))

    end_time = time.time()
    print "\nTotal time used in numpy : " + str(end_time - start_time)
    return max_eigenvalue, max_eigenvector

def is_proportional(a, b):
    assert(len(a) == len(b))
    eps = 1.0e-15
    norm_a = np.linalg.norm(a, 1)
    norm_b = np.linalg.norm(b, 1)
    if (norm_a < eps and norm_b < eps):
        return True
    if (norm_a * norm_b < eps):
        return False
    a_normalized = a/norm_a
    b_normalized = b/norm_b
    #print ", ".join(map(str, a_normalized))
    #print ", ".join(map(str, b_normalized))
    error = min(np.linalg.norm(a_normalized - b_normalized), np.linalg.norm(a_normalized + b_normalized))
    print "Difference of two leading eigenvectors = " + str(error)
    if error < eps:
        return True
    return False

def read_sparse_vector(sparse_vector_file_name, dimension):
    import os
    assert(os.path.exists(sparse_vector_file_name))
    data = []
    row_indices = []
    col_indices = [] 
    reader = open(sparse_vector_file_name, "r")
    for (index, string) in enumerate(reader):
        a = string.strip("\n").split(":")
        assert(len(a) == 2)
        row_indices.append(int(a[0]))
        col_indices.append(0)
        data.append(float(a[1]))
    reader.close()
    assert(dimension >= len(data))
    sparse_vector = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, 1))
    eps = 1.0e-15
    assert(abs(sum(sparse_vector) - 1) < eps)
    return sparse_vector

def save_sparse_vector(sparse_vector, output_file_name):
    vector = np.asarray(sparse_vector.todense())[:, 0]
    writer = open(output_file_name, "w")
    for i in range(len(vector)):
        writer.write(str(i) + ":" + str(vector[i]) + "\n")
    writer.close()

def analyze_transfer_matrix(transfer_matrix_file_name, alpha, personalization_file_name = "placeholder"):
    assert(os.path.exists(transfer_matrix_file_name))
    print "Reading transfer matrix ... "
    transfer_matrix = read_transfer_matrix(transfer_matrix_file_name)
    
    if personalization_file_name == "placeholder":
        leading_eigenvalue, leading_eigenvector = power_iteration(transfer_matrix, alpha)
        print "Leading eigenvalue from power iteration: " + str(leading_eigenvalue)
        #print "Leading eigenvector from power iteration: " + sparse_vector_to_string(leading_eigenvector)
        save_sparse_vector(leading_eigenvector, "page_rank_final_state.txt")
    else:
        row, col = transfer_matrix.shape
        assert(row == col)
        dimension = row
        personalization_vector = read_sparse_vector(personalization_file_name, dimension)
        leading_eigenvalue, leading_eigenvector = power_iteration_with_personalization(transfer_matrix, alpha, personalization_vector)
        print "Leading eigenvalue from power iteration: " + str(leading_eigenvalue)
        save_sparse_vector(leading_eigenvector, "page_rank_final_state.txt")
    return

    max_eigenvalue, max_eigenvector = get_leading_from_numpy(transfer_matrix, alpha)
    print "Leading eigenvalue from numpy : " + str(max_eigenvalue)
    print "\n\nEigenvalue difference for these two methods: " + str(abs(leading_eigenvalue - max_eigenvalue))
    print "Consistent leading eigenvector? " + str(is_proportional(max_eigenvector, np.asarray(leading_eigenvector.todense())[:, 0]))

def test_power_iteration(dimension, filling_factor):
    print "Generating a sparse matrix ... "
    sparse_matrix = generate_sparse_matrix(dimension, filling_factor)
    
    print "\nCalculating leading eigenvalue using two methods ... "
    leading_eigenvalue, leading_eigenvector = power_iteration(sparse_matrix, 1.0)
    print "Leading eigenvalue from power iteration: " + str(leading_eigenvalue)
    #print "Leading eigenvector from power iteration:\n " + sparse_vector_to_string(leading_eigenvector)

    max_eigenvalue, max_eigenvector = get_leading_from_numpy(sparse_matrix)
    #print "Leading eigenvector from numpy:\n " + ", ".join(map(str, max_eigenvector)) 
    print "\n\nError of leading eigenvalue for these two methods: " + str(abs(leading_eigenvalue - max_eigenvalue))
    print "Consistent leading eigenvector? " + str(is_proportional(max_eigenvector, np.asarray(leading_eigenvector.todense())[:, 0]))

def main_test():
    import sys
    if (len(sys.argv) != 3):
        print "dimension = sys.argv[1], filling_factor = sys.argv[2]. "
        return -1

    dimension = int(sys.argv[1])
    filling_factor  =float(sys.argv[2])
    test_power_iteration(dimension, filling_factor)
    return 0

def main():
    import sys
    if (len(sys.argv) < 3):
        print "transfer_matrix_file_name = sys.argv[1], alpha = sys.argv[2], personalize (optional) = sys.argv[3]. "
        return -1

    transfer_matrix_file_name = sys.argv[1]
    alpha = float(sys.argv[2])
    personalize = False
    if len(sys.argv) == 4:
        personalize = True
    if not personalize:
        analyze_transfer_matrix(transfer_matrix_file_name, alpha)
    else:
        personalization_file_name = "personalization_vector.txt"
        assert(os.path.exists(personalization_file_name))
        analyze_transfer_matrix(transfer_matrix_file_name, alpha, personalization_file_name)

if __name__ == "__main__":
    import sys
    sys.exit(main())
