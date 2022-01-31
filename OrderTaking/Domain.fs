namespace OrderTaking.Domain

open System

// TODO: Go deep in CE
// TODO: Go deep in Active Patterns

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


    type ShippingAddress = Undefined
    type BillingAddress = Undefined
    type Price = Undefined
    type BillingAmount = Undefined

    type Order =
        { Id: OrderId
          CustomerId: CustomerId
          ShippingAddress: ShippingAddress
          BillingAddress: BillingAddress
          OrderLines: OrderLine list
          AmountToBill: BillingAmount }

    and OrderLine =
        { Id: OrderLineId
          OrderId: OrderId
          ProductCode: ProductCode
          OrderQuantity: OrderQuantity
          Price: Price }

    type UnvalidatedCustomerInfo =
        { FirstName: string
          LastName: string
          EmailAddress: string }

    type UnvalidatedAddress =
        { AddressLine1: string
          AddressLine2: string
          AddressLine3: string
          AddressLine4: string
          City: string
          ZipCode: string }

    type UnvalidatedOrderLine =
        { OrderLineId: string
          ProductCode: string
          Quantity: decimal }

    type UnvalidatedOrder =
        { OrderId: string
          CustomerInfo: UnvalidatedCustomerInfo
          ShippingAddress: UnvalidatedAddress
          BillingAddress: UnvalidatedAddress
          Lines: UnvalidatedOrderLine list }

    // type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress,AddressValidationError>
    // type ValidateOrder =
    //   CheckProductCodeExists    // dependency​
    //     -> CheckAddressExists   // AsyncResult dependency​
    //     -> UnvalidatedOrder     // input​
    //     -> AsyncResult<ValidatedOrder,ValidationError list>  // output​
    type String50 = private String50 of string

    module String50 =

        /// Return the value inside a String50
        let value (String50 str) = str

        /// Create an String50 from a string
        /// Return Error if input is null, empty, or length > 50
        let create (str: string) =
            // ConstrainedType.createString fieldName String50 50 str
            String50 str

        /// Create an String50 from a string
        /// Return None if input is null, empty.
        /// Return error if length > maxLen
        /// Return Some if the input is valid
        let createOption (str: string) =
            // ConstrainedType.createStringOption fieldName String50 50 str
            Some(String50 str)

    type ZipCode = private ZipCode of string

    module ZipCode =
        let create (str: string) = ZipCode str

    type EmailAddress = private EmailAddress of string

    module EmailAddress =

        /// Return the string value inside an EmailAddress
        let value (EmailAddress str) = str

        /// Create an EmailAddress from a string
        /// Return Error if input is null, empty, or doesn't have an "@" in it
        let create (str: string) =
            // let pattern = ".+@.+" // anything separated by an "@"
            // ConstrainedType.createLike fieldName EmailAddress pattern str
            EmailAddress str

    type PersonalName =
        { FirstName: String50
          LastName: String50 }

    type CustomerInfo =
        { Name: PersonalName
          EmailAddress: EmailAddress }

    type Address =
        { AddressLine1: String50
          AddressLine2: String50 option
          AddressLine3: String50 option
          AddressLine4: String50 option
          City: String50
          ZipCode: ZipCode }

    type AddressValidationError =
        | InvalidFormat
        | AddressNotFound

    type ValidatedOrder =
        { OrderId: OrderId
          CustomerInfo: CustomerInfo
          ShippingAddress: Address
          BillingAddress: Address }

    type CheckProductCodeExists = ProductCode -> bool

    type CheckedAddress = CheckedAddress of UnvalidatedAddress

    type CheckAddressExists = UnvalidatedAddress -> CheckedAddress

    type ValidateOrder =
        CheckProductCodeExists // dependency​
            -> CheckAddressExists // AsyncResult dependency​
            -> UnvalidatedOrder // input​
            -> ValidatedOrder

    let toCustomerInfo (customer: UnvalidatedCustomerInfo) =
        let firstName = customer.FirstName |> String50.create
        let lastName = customer.LastName |> String50.create

        let emailAddress =
            customer.EmailAddress |> EmailAddress.create

        let name: PersonalName =
            { FirstName = firstName
              LastName = lastName }

        let customerInfo: CustomerInfo =
            { Name = name
              EmailAddress = emailAddress }

        customerInfo

    let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress =
        // call the remote service
        let checkedAddress = checkAddressExists unvalidatedAddress
        // extract the inner value using pattern matching
        let (CheckedAddress checkedAddress) = checkedAddress

        let addressLine1 =
            checkedAddress.AddressLine1 |> String50.create

        let addressLine2 =
            checkedAddress.AddressLine2
            |> String50.createOption

        let addressLine3 =
            checkedAddress.AddressLine3
            |> String50.createOption

        let addressLine4 =
            checkedAddress.AddressLine4
            |> String50.createOption

        let city = checkedAddress.City |> String50.create
        let zipCode = checkedAddress.ZipCode |> ZipCode.create

        let address: Address =
            { AddressLine1 = addressLine1
              AddressLine2 = addressLine2
              AddressLine3 = addressLine3
              AddressLine4 = addressLine4
              City = city
              ZipCode = zipCode }

        address

    let validateOrder: ValidateOrder =
        fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
            let orderId =
                unvalidatedOrder.OrderId |> OrderId.create

            let customerInfo =
                unvalidatedOrder.CustomerInfo |> toCustomerInfo

            let shippingAddress =
                unvalidatedOrder.ShippingAddress
                |> toAddress checkAddressExists

            let billingAddress =
                unvalidatedOrder.BillingAddress
                |> toAddress checkAddressExists
            // let lines =
            //     unvalidatedOrder.Lines
            //     |> List.map (toValidatedOrderLine checkProductCodeExists)
            let validatedOrder: ValidatedOrder =
                { OrderId = orderId
                  CustomerInfo = customerInfo
                  ShippingAddress = shippingAddress
                  BillingAddress = billingAddress
                //Lines = lines
                }

            validatedOrder
