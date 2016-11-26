open GameElements

type process =
  | CheckVehicleStatus of vehicle
  | BuyVehicle of vehicle
  | SellVehicle of vehicle
  | SetVehicleDestination of vehicle (*with updated destinations*)
  | CheckLocation of location
  | AddRoad of connection
  | DeleteRoad of connection
  | PurchaseRoad of connection (*Purchase rights to a road that is preexisting*)
  | CheckRoad of connection
  | Pause
  | EndGame
  | None


