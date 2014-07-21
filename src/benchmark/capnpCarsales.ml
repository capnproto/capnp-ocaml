
module CS = Carsales.Make(Capnp.BytesMessage)

module TestCase = struct
  type request_reader_t   = CS.Reader.ParkingLot.t
  type request_builder_t  = CS.Builder.ParkingLot.t
  type response_reader_t  = CS.Reader.TotalValue.t
  type response_builder_t = CS.Builder.TotalValue.t
  type expectation_t      = int

  let makes = [| "Toyota"; "GM"; "Ford"; "Honda"; "Tesla" |]
  let models = [| "Camry"; "Prius"; "Volt"; "Accord"; "Leaf"; "Model S" |]

  let color_of_int v =
    match v with
    | 0 -> CS.Reader.Color.Black
    | 1 -> CS.Reader.Color.White
    | 2 -> CS.Reader.Color.Red
    | 3 -> CS.Reader.Color.Green
    | 4 -> CS.Reader.Color.Blue
    | 5 -> CS.Reader.Color.Cyan
    | 6 -> CS.Reader.Color.Magenta
    | 7 -> CS.Reader.Color.Yellow
    | 8 -> CS.Reader.Color.Silver
    | _ -> failwith "bad color"


  let car_value car =
    let open CS.Reader.Car in
    let result = 0 in

    let result = result + (seats_get car) * 200 in
    let result = result + (doors_get car) * 350 in
    let result = Capnp.Array.fold (wheels_get car) ~init:result ~f:(fun acc wheel ->
        let diam  = CS.Reader.Wheel.diameter_get wheel in
        let diam2 = diam * diam in
        let st = if CS.Reader.Wheel.snow_tires_get wheel then 100 else 0 in
        acc + diam2 + st) in

    let result = result + ((length_get car) * (width_get car) * (height_get car) / 50) in

    let engine = engine_get car in
    let result = result + (CS.Reader.Engine.horsepower_get engine) * 40 in
    let result =
      if CS.Reader.Engine.uses_electric_get engine then
        if CS.Reader.Engine.uses_gas_get engine then
          result + 5000
        else
          result + 3000
      else
        result
    in

    let result = result + (if has_power_windows_get car then 100 else 0) in
    let result = result + (if has_power_steering_get car then 200 else 0) in
    let result = result + (if has_cruise_control_get car then 400 else 0) in
    let result = result + (if has_nav_system_get car then 2000 else 0) in

    let result = result + (cup_holders_get car) * 25 in
    result



  let random_car car =
    let open CS.Builder.Car in
    make_set car makes.(FastRand.int (Array.length makes));
    model_set car models.(FastRand.int (Array.length models));

    color_set car (color_of_int (FastRand.int 9));
    seats_set_exn car (2 + (FastRand.int 6));
    doors_set_exn car (2 + (FastRand.int 3));

    let wheels = wheels_init car 4 in
    Capnp.Array.iter wheels ~f:(fun wheel ->
      CS.Builder.Wheel.diameter_set_exn wheel (25 + (FastRand.int 15));
      CS.Builder.Wheel.air_pressure_set wheel (30.0 +. (FastRand.double 20.0));
      CS.Builder.Wheel.snow_tires_set wheel (FastRand.int 16 = 0));

    length_set_exn car (170 + (FastRand.int 150));
    width_set_exn car (48 + (FastRand.int 36));
    height_set_exn car (54 + (FastRand.int 48));
    let weight =
      (length_get car) * (width_get car) * (height_get car) / 200
    in
    weight_set_int_exn car weight;

    let engine = engine_init car in
    CS.Builder.Engine.horsepower_set_exn engine (100 * (FastRand.int 400));
    CS.Builder.Engine.cylinders_set_exn engine (4 + (2 * (FastRand.int 3)));
    CS.Builder.Engine.cc_set_int_exn engine (800 + (FastRand.int 10000));
    CS.Builder.Engine.uses_gas_set engine true;
    CS.Builder.Engine.uses_electric_set engine (FastRand.int 2 = 1);

    fuel_capacity_set car (10.0 +. (FastRand.double 30.0));
    fuel_level_set car (FastRand.double (fuel_capacity_get car));
    has_power_windows_set car (FastRand.int 2 = 1);
    has_power_steering_set car (FastRand.int 2 = 1);
    has_cruise_control_set car (FastRand.int 2 = 1);
    cup_holders_set_exn car (FastRand.int 12);
    has_nav_system_set car (FastRand.int 2 = 1)


  let setup_request () =
    let builder = CS.Builder.ParkingLot.init_root () in
    Capnp.Array.fold (CS.Builder.ParkingLot.cars_init builder (FastRand.int 200))
      ~init:(builder, 0)
      ~f:(fun (parking_lot, value) car ->
        let () = random_car car in
        (parking_lot, value + (car_value (CS.Reader.Car.of_builder car))))


  let handle_request parking_lot =
    let total_value = CS.Builder.TotalValue.init_root () in
    let result = Capnp.Array.fold (CS.Reader.ParkingLot.cars_get parking_lot)
        ~init:0 ~f:(fun acc car -> acc + (car_value car))
    in
    let () = CS.Builder.TotalValue.amount_set_int_exn total_value result in
    total_value


  let check_response total_value expected =
    (CS.Reader.TotalValue.amount_get_int_exn total_value) = expected

end

