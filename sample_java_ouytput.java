import java.io.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Java conversion of COBOL ORDER-PROCESSOR program
 * Processes customer orders with validation, credit checking, and reporting
 */
public class OrderProcessor {
    
    // Constants (equivalent to COBOL WS-CONSTANTS)
    private static final BigDecimal TAX_RATE = new BigDecimal("0.085");
    private static final int MAX_LINES_PER_PAGE = 55;
    private static final int MAX_ITEMS_PER_ORDER = 5;
    
    // File paths
    private static final String CUSTOMER_FILE = "custmast.dat";
    private static final String ORDER_FILE = "orderinp.dat";
    private static final String REPORT_FILE = "rptout.txt";
    
    // Counters and totals
    private int ordersProcessed = 0;
    private int ordersRejected = 0;
    private BigDecimal totalRevenue = BigDecimal.ZERO;
    private int reportLineCount = 0;
    
    // Customer database (simulating indexed file)
    private Map<String, CustomerRecord> customerDatabase;
    
    // Report writer
    private PrintWriter reportWriter;
    
    // Date formatter
    private final DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy");
    private final NumberFormat currencyFormatter = new DecimalFormat("$#,##0.00");
    
    public static void main(String[] args) {
        OrderProcessor processor = new OrderProcessor();
        try {
            processor.processOrders();
            System.out.println("Order processing completed successfully.");
        } catch (Exception e) {
            System.err.println("Error during order processing: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Main processing method (equivalent to 0000-MAIN-PROCESS)
     */
    public void processOrders() throws IOException {
        initialize();
        
        try (BufferedReader orderReader = Files.newBufferedReader(Paths.get(ORDER_FILE))) {
            String orderLine;
            while ((orderLine = orderReader.readLine()) != null) {
                OrderRecord order = parseOrderRecord(orderLine);
                if (order != null) {
                    processOrder(order);
                }
            }
        }
        
        finalize();
    }
    
    /**
     * Initialize system (equivalent to 1000-INITIALIZE)
     */
    private void initialize() throws IOException {
        // Load customer database
        customerDatabase = loadCustomerDatabase();
        
        // Open report file
        reportWriter = new PrintWriter(new FileWriter(REPORT_FILE));
        
        // Print report headers
        printHeaders();
        
        System.out.println("System initialized. Processing orders...");
    }
    
    /**
     * Load customer database from file (simulating indexed file access)
     */
    private Map<String, CustomerRecord> loadCustomerDatabase() throws IOException {
        Map<String, CustomerRecord> customers = new ConcurrentHashMap<>();
        
        try (BufferedReader reader = Files.newBufferedReader(Paths.get(CUSTOMER_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                CustomerRecord customer = parseCustomerRecord(line);
                if (customer != null && "A".equals(customer.status)) {
                    customers.put(customer.customerId, customer);
                }
            }
        } catch (NoSuchFileException e) {
            System.out.println("Customer file not found. Creating empty database.");
            // In a real system, you might create sample data here
        }
        
        return customers;
    }
    
    /**
     * Parse customer record from file line
     */
    private CustomerRecord parseCustomerRecord(String line) {
        if (line == null || line.length() < 200) return null;
        
        try {
            CustomerRecord customer = new CustomerRecord();
            customer.customerId = line.substring(0, 10).trim();
            customer.name = line.substring(10, 40).trim();
            customer.address = line.substring(40, 90).trim();
            customer.creditLimit = new BigDecimal(line.substring(90, 99)).divide(new BigDecimal("100"));
            customer.currentBalance = new BigDecimal(line.substring(99, 108)).divide(new BigDecimal("100"));
            customer.discountRate = new BigDecimal(line.substring(108, 111)).divide(new BigDecimal("1000"));
            customer.status = line.substring(111, 112);
            customer.lastOrderDate = line.substring(112, 120);
            
            return customer;
        } catch (Exception e) {
            System.err.println("Error parsing customer record: " + line);
            return null;
        }
    }
    
    /**
     * Parse order record from file line
     */
    private OrderRecord parseOrderRecord(String line) {
        if (line == null || line.length() < 150) return null;
        
        try {
            OrderRecord order = new OrderRecord();
            order.customerId = line.substring(0, 10).trim();
            order.orderNumber = line.substring(10, 22).trim();
            order.orderDate = line.substring(22, 30).trim();
            order.itemCount = Integer.parseInt(line.substring(30, 32).trim());
            
            order.items = new ArrayList<>();
            int pos = 32;
            for (int i = 0; i < Math.min(order.itemCount, MAX_ITEMS_PER_ORDER); i++) {
                OrderItem item = new OrderItem();
                item.itemCode = line.substring(pos, pos + 8).trim();
                item.quantity = Integer.parseInt(line.substring(pos + 8, pos + 13).trim());
                item.price = new BigDecimal(line.substring(pos + 13, pos + 20)).divide(new BigDecimal("100"));
                order.items.add(item);
                pos += 20;
            }
            
            return order;
        } catch (Exception e) {
            System.err.println("Error parsing order record: " + line);
            return null;
        }
    }
    
    /**
     * Process individual order (equivalent to 2000-PROCESS-ORDERS)
     */
    private void processOrder(OrderRecord order) {
        OrderProcessingResult result = new OrderProcessingResult();
        result.order = order;
        
        // Validate order
        if (!validateOrder(order, result)) {
            printRejectedOrder(result);
            ordersRejected++;
            return;
        }
        
        // Lookup customer
        CustomerRecord customer = customerDatabase.get(order.customerId);
        if (customer == null) {
            result.status = "CUSTOMER NOT FOUND";
            printRejectedOrder(result);
            ordersRejected++;
            return;
        }
        result.customer = customer;
        
        // Calculate order totals
        calculateOrder(order, customer, result);
        
        // Check credit limit
        if (!checkCreditLimit(customer, result)) {
            result.status = "CREDIT LIMIT EXCEEDED";
            printRejectedOrder(result);
            ordersRejected++;
            return;
        }
        
        // Update customer balance
        updateCustomer(customer, result);
        
        // Print approved order
        result.status = "APPROVED";
        printApprovedOrder(result);
        ordersProcessed++;
        totalRevenue = totalRevenue.add(result.finalAmount);
    }
    
    /**
     * Validate order data (equivalent to 2100-VALIDATE-ORDER)
     */
    private boolean validateOrder(OrderRecord order, OrderProcessingResult result) {
        if (order.customerId == null || order.customerId.isEmpty()) {
            result.status = "INVALID CUSTOMER ID";
            return false;
        }
        
        if (order.orderNumber == null || order.orderNumber.isEmpty()) {
            result.status = "INVALID ORDER NUMBER";
            return false;
        }
        
        if (order.itemCount <= 0 || order.itemCount > MAX_ITEMS_PER_ORDER) {
            result.status = "INVALID ITEM COUNT";
            return false;
        }
        
        if (order.items == null || order.items.size() != order.itemCount) {
            result.status = "ITEM COUNT MISMATCH";
            return false;
        }
        
        return true;
    }
    
    /**
     * Calculate order amounts (equivalent to 2300-CALCULATE-ORDER)
     */
    private void calculateOrder(OrderRecord order, CustomerRecord customer, OrderProcessingResult result) {
        // Calculate order total
        BigDecimal orderTotal = BigDecimal.ZERO;
        for (OrderItem item : order.items) {
            BigDecimal itemTotal = item.price.multiply(new BigDecimal(item.quantity));
            orderTotal = orderTotal.add(itemTotal);
        }
        result.orderTotal = orderTotal;
        
        // Calculate discount
        result.discountAmount = orderTotal.multiply(customer.discountRate);
        
        // Calculate net amount
        result.netAmount = orderTotal.subtract(result.discountAmount);
        
        // Calculate tax
        result.taxAmount = result.netAmount.multiply(TAX_RATE);
        
        // Calculate final amount
        result.finalAmount = result.netAmount.add(result.taxAmount);
    }
    
    /**
     * Check customer credit limit (equivalent to 2400-CHECK-CREDIT-LIMIT)
     */
    private boolean checkCreditLimit(CustomerRecord customer, OrderProcessingResult result) {
        BigDecimal newBalance = customer.currentBalance.add(result.finalAmount);
        BigDecimal availableCredit = customer.creditLimit.subtract(newBalance);
        
        return availableCredit.compareTo(BigDecimal.ZERO) >= 0;
    }
    
    /**
     * Update customer record (equivalent to 2500-UPDATE-CUSTOMER)
     */
    private void updateCustomer(CustomerRecord customer, OrderProcessingResult result) {
        customer.currentBalance = customer.currentBalance.add(result.finalAmount);
        customer.lastOrderDate = result.order.orderDate;
        
        // In a real system, you would write back to the database here
        customerDatabase.put(customer.customerId, customer);
    }
    
    /**
     * Print report headers (equivalent to 1200-PRINT-HEADERS)
     */
    private void printHeaders() {
        checkPageBreak();
        
        reportWriter.println();
        reportWriter.println("                                ORDER PROCESSING REPORT");
        reportWriter.println();
        reportWriter.printf("%-12s %-12s %-25s %-15s %-15s %-15s %s%n",
            "ORDER NO.", "CUSTOMER ID", "CUSTOMER NAME", "ORDER TOTAL", 
            "DISCOUNT", "NET AMOUNT", "STATUS");
        reportWriter.println("================================================================================");
        reportLineCount += 4;
    }
    
    /**
     * Print approved order detail (equivalent to 2600-PRINT-DETAIL-APPROVED)
     */
    private void printApprovedOrder(OrderProcessingResult result) {
        checkPageBreak();
        
        String customerName = result.customer.name.length() > 20 ? 
            result.customer.name.substring(0, 20) : result.customer.name;
        
        reportWriter.printf("%-12s %-12s %-25s %15s %15s %15s %s%n",
            result.order.orderNumber,
            result.order.customerId,
            customerName,
            currencyFormatter.format(result.orderTotal),
            currencyFormatter.format(result.discountAmount),
            currencyFormatter.format(result.finalAmount),
            result.status);
        
        reportLineCount++;
    }
    
    /**
     * Print rejected order detail (equivalent to 2700-PRINT-DETAIL-REJECTED)
     */
    private void printRejectedOrder(OrderProcessingResult result) {
        checkPageBreak();
        
        String customerName = result.customer != null ? 
            (result.customer.name.length() > 20 ? result.customer.name.substring(0, 20) : result.customer.name) :
            "UNKNOWN CUSTOMER";
        
        reportWriter.printf("%-12s %-12s %-25s %15s %15s %15s %s%n",
            result.order.orderNumber,
            result.order.customerId,
            customerName,
            "$0.00", "$0.00", "$0.00",
            result.status);
        
        reportLineCount++;
    }
    
    /**
     * Check for page break (equivalent to 2900-CHECK-PAGE-BREAK)
     */
    private void checkPageBreak() {
        if (reportLineCount > MAX_LINES_PER_PAGE) {
            newPage();
        }
    }
    
    /**
     * Start new page (equivalent to 2950-NEW-PAGE)
     */
    private void newPage() {
        reportWriter.println("\f"); // Form feed character
        printHeaders();
    }
    
    /**
     * Finalize processing and print summary (equivalent to 3000-FINALIZE)
     */
    private void finalize() {
        printSummary();
        
        if (reportWriter != null) {
            reportWriter.close();
        }
        
        // Save customer database back to file
        try {
            saveCustomerDatabase();
        } catch (IOException e) {
            System.err.println("Error saving customer database: " + e.getMessage());
