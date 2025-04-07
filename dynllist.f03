!------------------------------------------------------------
 ! Author : Crissy Grao
 ! Date : 6th April 2025
 ! Description : 
 ! I carried out the unbounded integers assignment using a dynamic linked list in Fortran. A digit is represented as a linked list of digits (in reverse) so that arithmetic operations can be easily carried out.
 ! Design Decisions:
 ! Implementation of Linked List: Each digit is coded in a node, starting with the least significant digit first. Addition, subtraction, multiplication, and division are made simple to achieve. Modularization: The program is divided into two files:
 ! dynllist.f03 to implement data types and arithmetic operations (including factorial) unbounded.f03 for the user interface.
 ! Input Validation: The input is cleansed to remove spaces and control characters and only valid digits (and a possible sign) are calculated.
 ! Zero Normalization: Zero is converted to positive in sign to avoid output like "-0".
 ! Challenges:
 ! Long division on a linked list was hard to do and to give correct results (e.g., 1/3 giving 0). Dynamic memory handling and support for negative numbers were properly taken care of by prudent design.
 ! How It Works:
 ! The operation to be done is selected by the user and operands are taken as input.
 ! The input is cleaned and transformed to a linked list representation (BigInt).
 ! The proper arithmetic function is invoked. During division, if the result is 0, the program expressly returns a BigInt of 0.
 ! The output is written to the terminal.
 ! Compilation and Execution:
 ! Compile with:
 ! gfortran -Wall dynllist.f03 unbounded.f03 -o unbounded 
 ! Run with:
 ! ./unbounded
!------------------------------------------------------------

module dynllist
  implicit none

  !------------------------------------------------------------
  ! Data Type Definitions
  !
  ! A Node holds one digit and a pointer to the next digit.
  ! BigInt stores a numbers sign and a linked list of digits.
  ! The digits are stored in reverse order
  ! to simplify arithmetic operations.

  type :: Node
     integer :: digit
     type(Node), pointer :: next => null()
  end type Node

  type :: BigInt
     integer :: sign           ! 1 for positive, -1 for negative
     type(Node), pointer :: head => null()   ! Head of the digit list
  end type BigInt

contains


  !------------------------------------------------------------
  ! Linked-List Helper Functions


  ! Create a new node with a given digit.
  function create_node(d) result(new_node)
    integer, intent(in) :: d
    type(Node), pointer :: new_node
    allocate(new_node)
    new_node%digit = d
    new_node%next => null()
  end function create_node


  ! Prepend a digit to a BigInt (insert at the front).
  subroutine prepend_digit(num, d)
    type(BigInt), intent(inout) :: num
    integer, intent(in) :: d
    type(Node), pointer :: new_node
    new_node => create_node(d)
    new_node%next => num%head
    num%head => new_node
  end subroutine prepend_digit


  ! Reverse the linked list of digits.
  subroutine reverse_bigint(num)
    type(BigInt), intent(inout) :: num
    type(Node), pointer :: prev, current, nxt
    prev => null()
    current => num%head
    do while (associated(current))
       nxt => current%next
       current%next => prev
       prev => current
       current => nxt
    end do
    num%head => prev
  end subroutine reverse_bigint


  ! Remove extra zeros at the beginning and ensure 0 is positive.
  subroutine remove_leading_zeros(num)
    type(BigInt), intent(inout) :: num
    type(Node), pointer :: current
    call reverse_bigint(num)
    current => num%head
    do while ( associated(current) .and. current%digit == 0 .and. associated(current%next) )
       num%head => current%next
       deallocate(current)
       current => num%head
    end do
    call reverse_bigint(num)
    ! If the number is 0, force the sign to be positive.
    if (associated(num%head)) then
       if (num%head%digit == 0 .and. .not. associated(num%head%next)) then
          num%sign = 1
       end if
    end if
  end subroutine remove_leading_zeros


  ! Copy a linked list from source to destination.
  subroutine copy_list(source, dest)
    type(Node), pointer, intent(in) :: source
    type(Node), pointer, intent(out) :: dest
    type(Node), pointer :: current_src, tail, new_node
    dest => null()
    tail => null()
    current_src => source
    do while (associated(current_src))
       new_node => create_node(current_src%digit)
       if (.not. associated(dest)) then
          dest => new_node
          tail => new_node
       else
          tail%next => new_node
          tail => new_node
       end if
       current_src => current_src%next
    end do
  end subroutine copy_list

  !------------------------------------------------------------
  ! Input/Output Functions


  ! Convert a string to a BigInt.
  ! This function removes all control characters (ASCII ≤ 32)
  ! so that only digits (and an optional sign) are processed.
  function build_bigint(numStr) result(num)
    character(len=*), intent(in) :: numStr
    type(BigInt) :: num
    integer :: i, d, nLen
    character :: ch
    character(len=:), allocatable :: cleanStr

    nLen = len(numStr)
    cleanStr = ""
    do i = 1, nLen
       ch = numStr(i:i)
       if (ichar(ch) > 32) then
          cleanStr = cleanStr // ch
       end if
    end do

    if (len_trim(cleanStr) == 0) then
       print *, "Error: empty input after cleaning."
       stop
    end if

    num%sign = 1
    num%head => null()
    i = 1
    if (cleanStr(1:1) == '-' .or. cleanStr(1:1) == '+') then
       if (cleanStr(1:1) == '-') num%sign = -1
       i = 2
    end if
    do while (i <= len_trim(cleanStr))
       ch = cleanStr(i:i)
       if (ichar(ch) < 48 .or. ichar(ch) > 57) then
         print *, "Error: Invalid character in input."
         stop
       end if
       d = ichar(ch) - ichar('0')
       call prepend_digit(num, d)
       i = i + 1
    end do
    call remove_leading_zeros(num)
  end function build_bigint

  ! Print the BigInt in normal order.
  recursive subroutine print_list_reverse(nptr)
    type(Node), pointer, intent(in) :: nptr
    if (.not. associated(nptr)) return
    call print_list_reverse(nptr%next)
    write(*, '(I1)', advance="no") nptr%digit
  end subroutine print_list_reverse

  subroutine print_bigint(num)
    type(BigInt), intent(in) :: num
    if (num%sign < 0) then
       write(*, '(A)', advance="no") "-"
    end if
    call print_list_reverse(num%head)
    print *
  end subroutine print_bigint


  !------------------------------------------------------------
  ! Helper Functions
 

  ! Return the number of digits in the BigInt.
  function length_bigint(num) result(len)
    type(BigInt), intent(in) :: num
    integer :: len
    type(Node), pointer :: current
    len = 0
    current => num%head
    do while (associated(current))
       len = len + 1
       current => current%next
    end do
  end function length_bigint


  ! Fill an array with the digits of BigInt in normal order.
  subroutine fill_digit_array(num, arr)
    type(BigInt), intent(in) :: num
    integer, intent(out) :: arr(:)
    integer :: i, len
    type(Node), pointer :: cur
    len = size(arr)
    cur => num%head
    i = len
    do while (associated(cur) .and. i >= 1)
       arr(i) = cur%digit
       i = i - 1
       cur => cur%next
    end do
  end subroutine fill_digit_array


  ! Compare two BigInts by their absolute values.
  ! Returns 1 if bigA > bigB, -1 if bigA < bigB, and 0 if equal.
  function compare_abs_bigint(bigA, bigB) result(comp)
    type(BigInt), intent(in) :: bigA, bigB
    integer :: comp, la, lb, i
    integer, allocatable :: arrA(:)
    integer, allocatable :: arrB(:)
    la = length_bigint(bigA)
    lb = length_bigint(bigB)
    if (la > lb) then
       comp = 1
       return
    else if (la < lb) then
       comp = -1
       return
    end if
    allocate(arrA(la))
    allocate(arrB(la))
    call fill_digit_array(bigA, arrA)
    call fill_digit_array(bigB, arrB)
    do i = 1, la
       if (arrA(i) > arrB(i)) then
          comp = 1
          deallocate(arrA)
          deallocate(arrB)
          return
       else if (arrA(i) < arrB(i)) then
          comp = -1
          deallocate(arrA)
          deallocate(arrB)
          return
       end if
    end do
    comp = 0
    deallocate(arrA)
    deallocate(arrB)
  end function compare_abs_bigint


  !------------------------------------------------------------
  ! Arithmetic Functions (for nonnegative BigInts)


  ! Add two BigInts (assumed nonnegative).
  function add_bigint(a, b) result(sum)
    type(BigInt), intent(in) :: a, b
    type(BigInt) :: sum
    type(Node), pointer :: pa, pb, tail, new_node
    integer :: carry, s, d
    carry = 0
    sum%sign = 1
    sum%head => null()
    tail => null()
    pa => a%head
    pb => b%head
    do while (associated(pa) .or. associated(pb) .or. carry /= 0)
       s = carry
       if (associated(pa)) then
          s = s + pa%digit
          pa => pa%next
       end if
       if (associated(pb)) then
          s = s + pb%digit
          pb => pb%next
       end if
       d = mod(s, 10)
       carry = s / 10
       new_node => create_node(d)
       if (.not. associated(sum%head)) then
          sum%head => new_node
          tail => new_node
       else
          tail%next => new_node
          tail => new_node
       end if
    end do
  end function add_bigint


  ! Multiply a BigInt by a single digit.
  function multiply_by_digit(a, d) result(prod)
    type(BigInt), intent(in) :: a
    integer, intent(in) :: d
    type(BigInt) :: prod
    type(Node), pointer :: pa, tail, new_node
    integer :: carry, s, digit
    carry = 0
    prod%sign = 1
    prod%head => null()
    tail => null()
    pa => a%head
    do while (associated(pa) .or. carry /= 0)
       if (associated(pa)) then
          s = pa%digit * d + carry
          pa => pa%next
       else
          s = carry
       end if
       digit = mod(s, 10)
       carry = s / 10
       new_node => create_node(digit)
       if (.not. associated(prod%head)) then
          prod%head => new_node
          tail => new_node
       else
          tail%next => new_node
          tail => new_node
       end if
    end do
  end function multiply_by_digit


  ! Shift a BigInt left by n digits (add n zeros at the front).
  function shift_left(a, n) result(shifted)
    type(BigInt), intent(in) :: a
    integer, intent(in) :: n
    type(BigInt) :: shifted
    integer :: i
    type(Node), pointer :: new_node
    if (length_bigint(a) == 1 .and. a%head%digit == 0) then
       shifted = a
    else
       shifted%sign = a%sign
       shifted%head => null()
       call copy_list(a%head, shifted%head)
       do i = 1, n
          new_node => create_node(0)
          new_node%next => shifted%head
          shifted%head => new_node
       end do
    end if
  end function shift_left


  ! Multiply two BigInts.
  function multiply_bigint(a, b) result(prod)
    type(BigInt), intent(in) :: a, b
    type(BigInt) :: prod, partial, abs_a, abs_b
    type(Node), pointer :: pb
    integer :: offset, d
    abs_a = a; abs_a%sign = 1
    abs_b = b; abs_b%sign = 1
    prod%head => create_node(0)
    offset = 0
    pb => abs_b%head
    do while (associated(pb))
       d = pb%digit
       partial = multiply_by_digit(abs_a, d)
       partial = shift_left(partial, offset)
       prod = add_bigint(prod, partial)
       offset = offset + 1
       pb => pb%next
    end do
    call remove_leading_zeros(prod)
    if (associated(prod%head)) then
       if (prod%head%digit == 0 .and. .not. associated(prod%head%next)) then
          prod%sign = 1
       else
          prod%sign = a%sign * b%sign
       end if
    else
       prod%sign = 1
    end if
  end function multiply_bigint


  ! Subtract two nonnegative BigInts (assuming a >= b).
  function subtract_nonnegative(a, b) result(diff)
    type(BigInt), intent(in) :: a, b
    type(BigInt) :: diff
    type(Node), pointer :: pa, pb, tail, new_node
    integer :: borrow, temp
    borrow = 0
    diff%sign = 1
    diff%head => null()
    tail => null()
    pa => a%head
    pb => b%head
    do while (associated(pa))
       temp = pa%digit - borrow
       if (associated(pb)) then
          temp = temp - pb%digit
          pb => pb%next
       end if
       if (temp < 0) then
          temp = temp + 10
          borrow = 1
       else
          borrow = 0
       end if
       new_node => create_node(temp)
       if (.not. associated(diff%head)) then
          diff%head => new_node
          tail => new_node
       else
          tail%next => new_node
          tail => new_node
       end if
       pa => pa%next
    end do
    call remove_leading_zeros(diff)
  end function subtract_nonnegative


  !------------------------------------------------------------
  ! Signed Arithmetic Wrappers


  ! Add two BigInts (handles the sign).
  function big_add(a, b) result(sum)
    type(BigInt), intent(in) :: a, b
    type(BigInt) :: sum, abs_a, abs_b
    if (a%sign == b%sign) then
       abs_a = a; abs_a%sign = 1
       abs_b = b; abs_b%sign = 1
       sum = add_bigint(abs_a, abs_b)
       sum%sign = a%sign
    else
       abs_a = a; abs_a%sign = 1
       abs_b = b; abs_b%sign = 1
       if (compare_abs_bigint(abs_a, abs_b) >= 0) then
          sum = subtract_nonnegative(abs_a, abs_b)
          sum%sign = a%sign
       else
          sum = subtract_nonnegative(abs_b, abs_a)
          sum%sign = b%sign
       end if
    end if
  end function big_add


  ! Subtract two BigInts (handles the sign)
  function big_sub(a, b) result(diff)
    type(BigInt), intent(in) :: a, b
    type(BigInt) :: diff, neg_b
    neg_b = b
    neg_b%sign = -neg_b%sign
    diff = big_add(a, neg_b)
  end function big_sub

  !------------------------------------------------------------
  ! Conversion Routines


  ! Convert a standard integer to a BigInt
  function int_to_big(n) result(big)
    integer, intent(in) :: n
    type(BigInt) :: big
    character(len=32) :: str
    write(str, '(I0)') n
    big = build_bigint(trim(str))
  end function int_to_big


  ! Convert a BigInt to a standard integer
  function big_to_int(n) result(val)
    type(BigInt), intent(in) :: n
    integer :: val, i, len
    integer, allocatable :: digits(:)
    val = 0
    len = length_bigint(n)
    allocate(digits(len))
    call fill_digit_array(n, digits)
    do i = 1, len
       val = val*10 + digits(i)
    end do
    deallocate(digits)
  end function big_to_int


  !------------------------------------------------------------
  ! Factorial Routine


  ! Calculate n! using BigInts
  ! this function only supports n up to 42
  function big_factorial(n) result(fact)
    type(BigInt), intent(in) :: n
    type(BigInt) :: fact, multiplier
    integer :: i, limit
    limit = big_to_int(n)
    if (limit < 0) then
       print *, "Factorial not defined for negative integers."
       stop
    else if (limit > 42) then
       print *, "Error: Factorial is implemented only up to 42!"
       stop
    end if
    fact = int_to_big(1)
    do i = 2, limit
       multiplier = int_to_big(i)
       fact = multiply_bigint(fact, multiplier)
    end do
  end function big_factorial

  !------------------------------------------------------------
  ! Division Routine (Long Division)


  ! Divide two BigInts and return the quotient
  function big_div(a, b) result(quot)
    type(BigInt), intent(in) :: a, b
    type(BigInt) :: quot, abs_a, abs_b, current, temp
    integer :: count, comp, i, len
    integer, allocatable :: arr(:)
    character(len=1000) :: qstr
    character(len=1) :: digitStr
    if (.not. associated(b%head)) then
       print *, "Division by zero error."
       stop
    end if
    abs_a = a; abs_a%sign = 1
    abs_b = b; abs_b%sign = 1
    quot%sign = a%sign * b%sign
    current%head => create_node(0)
    current%sign = 1
    len = length_bigint(abs_a)
    allocate(arr(len))
    call fill_digit_array(abs_a, arr)
    qstr = ""
    do i = 1, len
       current = shift_left(current, 1)
       temp%head => create_node(arr(i))
       temp%sign = 1
       current = big_add(current, temp)
       count = 0
       do
          comp = compare_abs_bigint(current, abs_b)
          if (comp < 0) exit
          current = subtract_nonnegative(current, abs_b)
          count = count + 1
       end do
       if (count == 0) then
          digitStr = "0"
       else
          write(digitStr, '(I1)') count
       end if
       qstr = trim(qstr)//trim(digitStr)
    end do
    if (len_trim(qstr) == 0) then
       qstr = "0"
    end if
    quot = build_bigint(trim(qstr))
    quot%sign = a%sign * b%sign
    deallocate(arr)
  end function big_div

end module dynllist
