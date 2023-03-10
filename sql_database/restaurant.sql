-- HomeWork_Sprint SQL
-- Restaurant Owners
-- 5 Tables
-- 1x Fact , 4x Dimension
-- search google , how to add foreign key
-- write SQL 3 queries analyze data
-- 1x subquery / with


--sqlite command
.mode markdown
.header on 
 --Dimentions #1 
CREATE TABLE dim_customer(
  customer_id INT PRIMARY KEY,
  first_name VARCHAR,
  last_name VARCHAR,
  company_name VARCHAR,
  location VARCHAR,
  country VARCHAR
  );

INSERT INTO dim_customer VALUES 
  (1,'Harry','Maguire','AAA Company','Manchester','England'),
  (2,'Jesse','Lingard','ABB Corperate','Notthingham','England'),
  (3,'Phile','Jones','BBA Company','Manchester','England');

--Dimentions #2
CREATE TABLE dim_employee(
  employee_id INT PRIMARY KEY,
  fisrt_name VARCHAR,
  last_name VARCHAR,
  department VARCHAR,
  category VARCHAR,
  salary REAL,
  age REAL,
  country VARCHAR
);

INSERT INTO dim_employee VALUES 
(1,'Raphael','Varane','Operations','Banana Cake',35000,29,'France'),
(2,'Ji-Sung','Park','Operations','Sheet Cake',32000,26,'Korea'),
(3,'Antony','Martial','Operations','Croissant',42000,35,'France'),
(4,'Shinji','Kagawa','Operations','Butter Bread',55000,48,'Japan')
;
--Dimentions #3
CREATE TABLE dim_menu(
 menu_id INT PRIMARY KEY,
 menu_name VARCHAR ,
 menu_price REAL
);

INSERT INTO dim_menu VALUES 
(1,'Banana Cake',80),
(2,'Sheet Cake',175),
(3,'Croissant',90),
(4,'Butter Bread',230);
--Dimentions #4
CREATE TABLE dim_payment(
 payment_id INT PRIMARY KEY,
 payment_method VARCHAR
);

INSERT INTO dim_payment VALUES 
(1,'Cash'),
(2,'Debit Cards'),
(3,'Mobile Payments'),
(4,'Electronic Bank Transfers');

--Fact Table #1
CREATE TABLE fact_order(
order_id INT PRIMARY KEY,
order_date DATE,
quality REAL,
menu_id INT,
employee_id INT,
customer_id INT,
payment_id INT,
FOREIGN KEY (menu_id) REFERENCES menu(menu_id),
FOREIGN KEY (employee_id) REFERENCES employee(employee_id),
FOREIGN KEY (customer_id) REFERENCES customer(customer_id),
FOREIGN KEY (payment_id) REFERENCES payment(payment_id)
);

INSERT INTO fact_order VALUES 
(1,'2022-08-07',50,1,1,1,1),
(2,'2022-08-07',120,3,3,2,2),
(3,'2022-08-21',65,2,2,3,3),
(4,'2022-08-21',75,4,4,2,2),
(5,'2022-09-07',60,1,1,2,4),
(6,'2022-09-07',120,3,3,3,2),
(7,'2022-09-21',80,2,2,3,3),
(8,'2022-09-21',40,2,2,1,2),
(9,'2022-10-07',55,1,1,1,4),
(10,'2022-10-07',35,3,3,1,2),
(11,'2022-10-21',40,4,4,2,3),
(12,'2022-10-21',60,4,4,3,2),
(13,'2022-11-07',100,1,1,2,4),
(14,'2022-11-07',60,1,1,1,3),
(15,'2022-11-21',70,4,4,3,2),
(16,'2022-11-21',60,3,3,1,3),
(17,'2022-12-07',80,3,3,2,2),
(18,'2022-12-07',100,3,3,3,4),
(19,'2022-12-21',150,2,2,2,4),
(20,'2022-12-21',150,2,2,1,2)
;
-- Most Popular Menu
WITH sub AS (
SELECT 
    menu.menu_name AS Name,
    Bill.quality AS EA,
    menu.menu_price AS unitprice,
    bill.quality * menu.menu_price AS grandtotal
FROM fact_order AS bill
JOIN dim_menu AS menu
WHERE Bill.menu_id = menu.menu_id
)

SELECT 
     Name AS Menu_Name,
     SUM(GRANDTOTAL) AS Amout
FROM  sub
GROUP by 1
ORDER BY 2 DESC;

--Query #2
WITH grandmonth AS (
SELECT 
    STRFTIME('%Y-%m',Bill.order_date) AS MonthID,
    bill.quality * menu.menu_price AS grandtotal
FROM fact_order AS bill
JOIN dim_menu AS menu
WHERE Bill.menu_id = menu.menu_id)

SELECT 
  MonthID,
  SUM(grandtotal) AS Amount
FROM grandmonth
GROUP by MonthID
ORDER BY SUM(grandtotal) DESC;

--Query #3
SELECT 
    dim_payment.payment_method AS MethodID,
    SUM(fact_order.quality*menu_price) AS Amout
FROM fact_order
JOIN dim_menu 
ON  fact_order.menu_id = dim_menu.menu_id
JOIN dim_payment
ON fact_order.payment_id = dim_payment.payment_id
GROUP BY dim_payment.payment_method
ORDER BY SUM(fact_order.quality*menu_price) DESC;

--Query #4
SELECT 
    dim_employee.fisrt_name AS Firstname,
    dim_employee.last_name AS Lastname,
    dim_employee.age AS Age,
    dim_employee.category as Category,
    SUM(fact_order.quality * dim_menu.menu_price) as 'Revenue',
    dim_employee.salary AS Salary,
CASE 
 WHEN SUM(fact_order.quality * dim_menu.menu_price) >= 75000 THEN dim_employee.salary+(dim_employee.salary*0.15)
 WHEN SUM(fact_order.quality * dim_menu.menu_price) BETWEEN 50000 AND 75000 THEN dim_employee.salary+(dim_employee.salary*0.10)
 WHEN SUM(fact_order.quality * dim_menu.menu_price) <= 50000 THEN dim_employee.salary+(dim_employee.salary*0.05)
  ELSE dim_employee.salary
   END AS New_Salary
FROM fact_order,dim_menu,dim_employee
WHERE fact_order.menu_id = dim_menu.menu_id
AND   fact_order.employee_id = dim_employee.employee_id
GROUP BY dim_employee.fisrt_name,dim_employee.last_name
ORDER BY New_Salary DESC;
