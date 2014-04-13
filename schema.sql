CREATE TABLE clients (
       pk INTEGER PRIMARY KEY,
       id INTEGER,
       name text,
       street text,
       zip text,
       city text,
       state text,
       country text,
       vatNo text
);
CREATE TABLE products (
       pk INTEGER PRIMARY KEY,
       name text,
       priceDecimalPlaces INTEGER, price INTEGER,
       base INTEGER, -- 1 for units, 60 for time based (minutes), ...
       currency text
);
CREATE TABLE invoices (
       pk INTEGER PRIMARY KEY,
       id INTEGER,
       client INTEGER,
       `from` TIMESTAMP, `to` TIMESTAMP,
       `date` TIMESTAMP,
       `due` TIMESTAMP,
       vatDecimalPlaces INTEGER, vat INTEGER,
       discountDecimalPlaces INTEGER, discount INTEGER, 
       cashbackDecimalPlaces INTEGER, cashback INTEGER
);
CREATE TABLE invoices_products (
       invoice INTEGER,
       product INTEGER,
       quantity INTEGER
);
