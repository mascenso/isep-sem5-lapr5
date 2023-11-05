import 'reflect-metadata';

import * as sinon from 'sinon';
import {Container} from "typedi";
import {Building} from "../../src/domain/building-agg/building";
import {Result} from "../../src/core/logic/Result";
import {Floor} from "../../src/domain/floor";
import RoomService from "../../src/services/roomService";
import {RoomType} from "../../src/domain/roomType";
import {Room} from "../../src/domain/room";


describe('Room Service', () => {
  const sandbox = sinon.createSandbox();

  beforeEach(function() {
    this.timeout(6000);
    Container.reset();
    let roomSchemaInstance = require("../../src/persistence/schemas/roomSchema").default;
    Container.set("roomSchema", roomSchemaInstance);

    let buildingRepoClass = require("../../src/repos/buildingRepo").default;
    let buildingRepoInstance = Container.get(buildingRepoClass);
    Container.set("BuildingRepo", buildingRepoInstance);

    let floorRepoClass = require("../../src/repos/floorRepo").default;
    let floorRepoInstance = Container.get(floorRepoClass);
    Container.set("FloorRepo", floorRepoInstance);

    let roomRepoClass = require("../../src/repos/roomRepo").default;
    let roomRepoInstance = Container.get(roomRepoClass);
    Container.set("RoomRepo", roomRepoInstance);

    let buildingInstance = require("../../src/domain/building-agg/building").Building;
    Container.set("Building", buildingInstance);

    let floorInstance = require("../../src/domain/floor").Floor;
    Container.set("Floor", floorInstance);

    let roomInstance = require("../../src/domain/room").Room;
    Container.set("Room", roomInstance);

    let roomServiceClass = require("../../src/services/roomService").default;
    let roomServiceInstance = Container.get(roomServiceClass);
    Container.set("RoomService", roomServiceInstance);

  });

  afterEach(function() {
    sinon.restore();
    sandbox.restore();
  });

  it('given valid floor and building ids when creating a room returns a valid room instance', async () => {
    // given
    const roomDTO = {
      buildingId: 'B123',
      floorId: 'F123',
      name: 'B306',
      description: 'Sala muito linda',
      roomType: 'office'
    };

    let buildingInstance = Container.get("Building");
    const buildingStub = sinon.stub(buildingInstance, "create").returns( Result.ok( {
      "id":"B123",
      "code": "B123",
      "maxWidth": 10,
      "maxLength": 20,
      "name": "Edificio 2",
      "description":"Edificio muito lindo"
    }
    ));

    let buildingRepoInstance = Container.get("BuildingRepo");
    const buildingRepoStub = sinon.stub(buildingRepoInstance, "findByDomainId")
      .returns(new Promise<Building>((resolve, reject) => {resolve(Building.create({
          "code": "B123",
          "maxWidth": 10,
          "maxLength": 20,
          "name": "Edificio 2",
          "description":"Edificio muito lindo"
        }).getValue()
        )})
      );

    let floorInstance = Container.get("Floor");
    const floorStub = sinon.stub(floorInstance, "create").returns( Result.ok( {
      "id":"F123",
      "buildingId": "B123",
      "floorNumber": 1,
      "width": 10,
      "length": 20,
      "floorMap": [[]],
      "description": "Floor falso muito lindo"}
    ));

    let floorRepoInstance = Container.get("FloorRepo");
    const floorRepoStub = sinon.stub(floorRepoInstance, "findByDomainId")
      .returns(new Promise<Floor>( (resolve, _) => {resolve(Floor.create({
          "buildingId": "B123",
          "floorNumber": 1,
          "width": 10,
          "length": 20,
          "floorMap": [[]],
          "description": "Floor falso muito lindo"}
      ).getValue())} ));

    let roomInstance = Container.get("Room");
    const roomStub = sinon.stub(roomInstance, "create").returns( Result.ok( {
      id: 'R123',
      buildingId: 'B123',
      floorId: 'F123',
      name: 'B306',
      description: 'Sala muito linda',
      roomType: RoomType.OFFICE
    }) );

    let roomRepoInstance = Container.get("RoomRepo");
    const roomRepoStub = sinon.stub(roomRepoInstance, "save")
      .returns(new Promise<Room>( (resolve, _) => {resolve(Room.create({
        buildingId: 'B123',
        floorId: 'F123',
        name: 'B306',
        description: 'Sala muito linda',
        roomType: RoomType.OFFICE}
      ).getValue())} ));

    const service = new RoomService(
      Container.get("RoomRepo"),
      Container.get("FloorRepo"),
      Container.get("BuildingRepo")
    );

    // when creating room
    const result = await service.createRoom({id: undefined, domainId:undefined, ...roomDTO});

    // then
    sinon.assert.called(buildingStub);
    sinon.assert.called(floorStub);
    sinon.assert.called(roomStub);
    sinon.assert.calledOnce(buildingRepoStub);
    sinon.assert.calledOnce(floorRepoStub);
    sinon.assert.calledOnce(roomRepoStub);
    sinon.assert.match(result.isSuccess, true);

  });

  it('given invalid floorId when creating a room returns an error', async () => {
    // given
    const roomDTO = {
      buildingId: 'B123',
      floorId: 'nonexistent floorId',
      name: 'B306',
      description: 'Sala muito linda',
      roomType: 'office'
    };

    let floorRepoInstance = Container.get("FloorRepo");
    const floorRepoStub = sinon.stub(floorRepoInstance, "findByDomainId")
      .returns(new Promise<Floor>( (resolve, _) => resolve( null)));


    const service = new RoomService(
      Container.get("RoomRepo"),
      Container.get("FloorRepo"),
      Container.get("BuildingRepo")
    );

    // when creating room
    const result = await service.createRoom({id: undefined, domainId:undefined, ...roomDTO});

    // then
    sinon.assert.calledOnce(floorRepoStub);
    sinon.assert.match(result.isFailure, true);
    sinon.assert.match(result.errorValue(), 'Floor with id nonexistent floorId not found!');
  });


  it('given invalid buildingId when creating a room returns an error', async () => {
    // given
    const roomDTO = {
      buildingId: 'id_obstroncio',
      floorId: 'F123',
      name: 'B306',
      description: 'Sala muito linda',
      roomType: 'office'
    };

    let floorInstance = Container.get("Floor");
    const floorStub = sinon.stub(floorInstance, "create").returns( Result.ok( {
      "id":"F123",
      "buildingId": "B123",
      "floorNumber": 1,
      "width": 10,
      "length": 20,
      "floorMap": [[]],
      "description": "Floor falso muito lindo"}
    ));

    let floorRepoInstance = Container.get("FloorRepo");
    const floorRepoStub = sinon.stub(floorRepoInstance, "findByDomainId")
      .returns(new Promise<Floor>( (resolve, _) => {resolve(Floor.create({
        "buildingId": "B123",
        "floorNumber": 1,
        "width": 10,
        "length": 20,
        "floorMap": [[]],
        "description": "Floor falso muito lindo"}
      ).getValue())} ));

    let buildingRepoInstance = Container.get("BuildingRepo");
    const buildingRepoStub = sinon.stub(buildingRepoInstance, "findByDomainId")
      .returns(new Promise<Building>((resolve, reject) => resolve(null)));

    const service = new RoomService(
      Container.get("RoomRepo"),
      Container.get("FloorRepo"),
      Container.get("BuildingRepo")
    );

    // when creating room
    const result = await service.createRoom({id: undefined, domainId:undefined, ...roomDTO});

    // then
    sinon.assert.called(floorStub);
    sinon.assert.calledOnce(buildingRepoStub);
    sinon.assert.calledOnce(floorRepoStub);
    sinon.assert.match(result.isFailure, true);
    sinon.assert.match(result.errorValue(), 'Building with id id_obstroncio not found!');

  });


  it('given existing roomId when finding room by id returns a valid room instance', async () => {
    // given
    let roomInstance = Container.get("Room");
    const roomStub = sinon.stub(roomInstance, "create").returns( Result.ok( {
      id: 'R123',
      buildingId: 'B123',
      floorId: 'F123',
      name: 'B306',
      description: 'Sala muito linda',
      roomType: RoomType.OFFICE
    }) );

    let roomRepoInstance = Container.get("RoomRepo");
    const roomRepoStub = sinon.stub(roomRepoInstance, "findByDomainId")
      .returns(new Promise<Room>( (resolve, _) => {resolve(Room.create({
        buildingId: 'B123',
        floorId: 'F123',
        name: 'B306',
        description: 'Sala muito linda',
        roomType: RoomType.OFFICE}
      ).getValue())} ));

    const service = new RoomService(
      Container.get("RoomRepo"),
      Container.get("FloorRepo"),
      Container.get("BuildingRepo")
    );

    // when creating room
    const result = await service.getRoomById("R123");

    // then
    sinon.assert.called(roomStub);
    sinon.assert.calledOnce(roomRepoStub);
    sinon.assert.match(result.isSuccess, true);
    sinon.assert.match(result.getValue().id, "R123");

  });

  it('given nonexistent roomId when finding room by id returns an error', async () => {
    // given
    let roomRepoInstance = Container.get("RoomRepo");
    const roomRepoStub = sinon.stub(roomRepoInstance, "findByDomainId")
      .returns(new Promise<Room>( (resolve, _) => {resolve(null)} ));

    const service = new RoomService(
      Container.get("RoomRepo"),
      Container.get("FloorRepo"),
      Container.get("BuildingRepo")
    );

    // when creating room
    const result = await service.getRoomById("R123");

    // then
    sinon.assert.calledOnce(roomRepoStub);
    sinon.assert.match(result.isFailure, true);
    sinon.assert.match(result.errorValue(), "Room with id R123 not found!");

  });

});
