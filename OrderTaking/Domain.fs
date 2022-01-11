namespace OrderTaking.Domain

open System

module Domain = 
    // Value Objects
    type WidgetCode = WidgetCode of string // W = 4 digits
    type GizmoCode = GizmoCode of string // G = 3 digits

    type ProductCode =
        | Widget of WidgetCode
        | Gizmo of GizmoCode

    type UnitQuantity = UnitQuantity of int
    type KilogramQuantity = KilogramQuantity of decimal

    type OrderQuantity =
        | Unit of UnitQuantity
        | Kilos of KilogramQuantity

    // Entities
    type OrderId = private OrderId of string
        
    module OrderId =
        /// Define a "Smart constructor" for OrderId​
        /// string -> OrderId​
        let create str = 
            if String.IsNullOrEmpty(str) then
                // use exceptions rather than Result for now​
                failwith "OrderId must not be null or empty"
            elif str.Length > 50 then
                failwith "OrderId must not be more than 50 chars"
            else
                OrderId str
                
        let value (OrderId str) = str

    type OrderLineId = Undefined
    type CustomerId = Undefined

    type CustomerInfo = Undefined
    type ShippingAddress = Undefined
    type BillingAddress = Undefined
    type Price = Undefined
    type BillingAmount = Undefined

    type Order = { 
        Id: OrderId
        CustomerId: CustomerId
        ShippingAddress: ShippingAddress
        BillingAddress: BillingAddress
        OrderLines: OrderLine list
        AmountToBill: BillingAmount 
    }

    and OrderLine = { 
        Id: OrderLineId
        OrderId: OrderId
        ProductCode: ProductCode
        OrderQuantity: OrderQuantity
        Price: Price 
    }

    type UnvalidatedOrder = {
        OrderId: string
        CustomerInfo: string
        ShippingAddress: string
    }

    // type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress,AddressValidationError>
    // type ValidateOrder =
    //   CheckProductCodeExists    // dependency​
    //     -> CheckAddressExists   // AsyncResult dependency​
    //     -> UnvalidatedOrder     // input​
    //     -> AsyncResult<ValidatedOrder,ValidationError list>  // output​
    type String50 = private String50 of string
    type ZipCode = private ZipCode of string
    type EmailAddress = private EmailAddress of string
    type Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode
    }

    type AddressValidationError = 
    | InvalidFormat 
    | AddressNotFound 

    type UnvalidatedAddress = {
    AddressLine1 : string
    AddressLine2 : string
    AddressLine3 : string
    AddressLine4 : string
    City : string
    ZipCode : string
    }

    type ValidatedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : Address
    BillingAddress : Address
    //Lines : ValidatedOrderLine list
    }

    type CheckProductCodeExists = ProductCode -> bool

    type CheckedAddress = CheckedAddress of UnvalidatedAddress

    type CheckAddressExists = UnvalidatedAddress -> CheckedAddress
    type ValidateOrder =
      CheckProductCodeExists    // dependency​
        -> CheckAddressExists   // AsyncResult dependency​
        -> UnvalidatedOrder     // input​
        -> ValidatedOrder       // output​

    let toCustomerInfo (customer: UnvalidatedCustomerInfo) =
        let firstName = customer.FirstName |> String50.create
        let lastName = customer.LastName |> String50.create
        let emailAddress = customer.EmailAddress |> EmailAddress.create
        let customerInfo : CustomerInfo = {
            Name = name
            EmailAddress = emailAddress
        }
        customerInfo 

    let toAddress (CheckedAddress checkedAddress) =
        result {
            let! addressLine1 = 
                checkedAddress.AddressLine1 
                |> String50.create "AddressLine1" 
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! addressLine2 = 
                checkedAddress.AddressLine2 
                |> String50.createOption "AddressLine2"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! addressLine3 = 
                checkedAddress.AddressLine3 
                |> String50.createOption "AddressLine3" 
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! addressLine4 = 
                checkedAddress.AddressLine4 
                |> String50.createOption "AddressLine4"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! city = 
                checkedAddress.City
                |> String50.create "City"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let! zipCode = 
                checkedAddress.ZipCode
                |> ZipCode.create "ZipCode"
                |> Result.mapError ValidationError // convert creation error into ValidationError
            let address : Address = {
                AddressLine1 = addressLine1
                AddressLine2 = addressLine2
                AddressLine3 = addressLine3
                AddressLine4 = addressLine4
                City = city
                ZipCode = zipCode
                }
            return address
        }

    let validateOrder : ValidateOrder =
      fun checkProductCodeExists checkAddressExists unvalidatedOrder ->

        let orderId =
            unvalidatedOrder.OrderId
            |> OrderId.create

        let customerInfo =
            unvalidatedOrder.CustomerInfo
            |> toCustomerInfo   // helper function​

        let shippingAddress =
            unvalidatedOrder.ShippingAddress
            |> toAddress       // helper function​

        // and so on, for each property of the unvalidatedOrder​

        // when all the fields are ready, use them to​
        // create and return a new "ValidatedOrder" record​
        {
            OrderId = orderId
            CustomerInfo = customerInfo
            ShippingAddress = shippingAddress
            BillingAddress = ...
            Lines = ...
        }
    
    

