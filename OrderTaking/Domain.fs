namespace OrderTaking.Domain

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
type OrderId = Undefined
type OrderLineId = Undefined
type CustomerId = Undefined

type CustomerInfo = Undefined
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

// type UnvalidatedOrder = {
//     OrderId: string
//     CustomerInfo: string
//     ShippingAddress: string
// }
