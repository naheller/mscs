def insertion_sort(A):
    for i in range(len(A)):
        j = i
        while j > 0 and (A[j] < A[j - 1]):
               temp = A[j]
               A[j] = A[j - 1]
               A[j - 1] = temp
               j = j - 1

my_list = [5, 4, 3, 2, 1]
insertion_sort(my_list)
print(my_list)
