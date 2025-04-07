program unbounded
  use dynllist
  implicit none
  character(len=100) :: op, operand1, operand2
  type(BigInt) :: num1, num2, result

  ! Prompt the user for an operation.
  print *, "Enter an operation: + - * / or !"
  read(*, '(A)') op

  select case (trim(op))
  case ("+")
     print *, "Enter first operand:"
     read(*, '(A)') operand1
     print *, "Enter second operand:"
     read(*, '(A)') operand2
     num1 = build_bigint(trim(operand1))
     num2 = build_bigint(trim(operand2))
     result = big_add(num1, num2)
     print *, "The result is:"
     call print_bigint(result)
  case ("-")
     print *, "Enter first operand:"
     read(*, '(A)') operand1
     print *, "Enter second operand:"
     read(*, '(A)') operand2
     num1 = build_bigint(trim(operand1))
     num2 = build_bigint(trim(operand2))
     result = big_sub(num1, num2)
     print *, "The result is:"
     call print_bigint(result)
  case ("*")
     print *, "Enter first operand:"
     read(*, '(A)') operand1
     print *, "Enter second operand:"
     read(*, '(A)') operand2
     num1 = build_bigint(trim(operand1))
     num2 = build_bigint(trim(operand2))
     result = multiply_bigint(num1, num2)
     print *, "The result is:"
     call print_bigint(result)
  case ("/")
     print *, "Enter first operand:"
     read(*, '(A)') operand1
     print *, "Enter second operand:"
     read(*, '(A)') operand2
     num1 = build_bigint(trim(operand1))
     num2 = build_bigint(trim(operand2))
     result = big_div(num1, num2)
     print *, "The result is:"
     call print_bigint(result)
  case ("!")
     print *, "Enter operand for factorial (max 42):"
     read(*, '(A)') operand1
     num1 = build_bigint(trim(operand1))
     result = big_factorial(num1)
     print *, "The factorial is:"
     call print_bigint(result)
  case default
     print *, "Invalid operation."
  end select

end program unbounded
