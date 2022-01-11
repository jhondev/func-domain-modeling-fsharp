namespace OrderTaking.Domain

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
        
    // module OrderId =
    //     /// Define a "Smart constructor" for OrderId​
    //     /// string -> OrderId​
    //     let create str = 
    //         if String.IsNullOrEmpty(str) then
    //             // use exceptions rather than Result for now​
    //             failwith "OrderId must not be null or empty"
    //         elif str.Length > 50 then
    //             failwith "OrderId must not be more than 50 chars"
    //         else
    //             OrderId str
                
    //     let value (OrderId str) = str

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
    
    // type CheckAddressExists = UnvalidatedAddress -> CheckedAddress
    // type ValidateOrder =
    //   CheckProductCodeExists    // dependency​
    //     -> CheckAddressExists   // AsyncResult dependency​
    //     -> UnvalidatedOrder     // input​
    //     -> ValidatedOrder       // output​

