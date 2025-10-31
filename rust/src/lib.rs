#[cfg(test)]
mod tests {
    #[test]
    fn test_basic_arithmetic() {
        assert_eq!(2 + 2, 4);
    }
    
    #[test]
    fn test_list_operations() {
        let numbers = vec![1, 2, 3, 4, 5];
        let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
        assert_eq!(doubled, vec![2, 4, 6, 8, 10]);
    }
    
    #[test]
    fn test_higher_order_functions() {
        let numbers = vec![1, 2, 3, 4, 5];
        let sum: i32 = numbers.iter().sum();
        assert_eq!(sum, 15);
    }
    
    #[test]
    fn test_closures() {
        let multiply = |x: i32, y: i32| x * y;
        assert_eq!(multiply(3, 4), 12);
    }
}
