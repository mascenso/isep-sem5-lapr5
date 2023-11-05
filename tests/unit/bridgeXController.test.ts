import "reflect-metadata";

import * as sinon from "sinon";
import { expect } from 'chai';
import { Response, Request, NextFunction } from "express";
import { Container } from "typedi";
import { Result } from "../../src/core/logic/Result";
import IBridgeService from "../../src/services/IServices/IBridgeService";
import BridgeController from "../../src/controllers/bridgeController";
import IBridgeDTO from "../../src/dto/IBridgeDTO";
import { Bridge } from "../../src/domain/bridge-agg/bridge";

describe("bridge controller", function() {


  const sandbox = sinon.createSandbox();
  beforeEach(function() {
    this.timeout(9000);
    Container.reset();
    let buildingSchemaInstance = require("../../src/persistence/schemas/buildingSchema").default;
    Container.set("buildingSchema", buildingSchemaInstance);

    let elevatorSchemaClass = require("../../src/persistence/schemas/elevatorSchema").default;
    Container.set("elevatorSchema", elevatorSchemaClass);

    let floorSchemaInstance = require("../../src/persistence/schemas/floorSchema").default;
    Container.set("floorSchema", floorSchemaInstance);

    let buildingRepoClass = require("../../src/repos/buildingRepo").default;
    let buildingRepoInstance = Container.get(buildingRepoClass);
    Container.set("BuildingRepo", buildingRepoInstance);

    let floorRepoClass = require("../../src/repos/floorRepo").default;
    let floorRepoInstance = Container.get(floorRepoClass);
    Container.set("FloorRepo", floorRepoInstance);

    let elevatorRepoClass = require("../../src/repos/elevatorRepo").default;
    let elevatorRepoInstance = Container.get(elevatorRepoClass);
    Container.set("ElevatorRepo", elevatorRepoInstance);

    let buildingInstance = require("../../src/domain/building").Building;
    Container.set("Building", buildingInstance);

    let floorInstance = require("../../src/domain/floor").Floor;
    Container.set("Floor", floorInstance);

    let floorServiceClass = require("../../src/services/floorService").default;
    let floorServiceInstance = Container.get(floorServiceClass);
    Container.set("FloorService", floorServiceInstance);

    let bridgeSchemaInstance = require("../../src/persistence/schemas/bridgeSchema").default;
    Container.set("bridgeSchema", bridgeSchemaInstance);

    let bridgeRepoClass = require("../../src/repos/bridgeRepo").default;
    let bridgeRepoInstance = Container.get(bridgeRepoClass);
    Container.set("BridgeRepo", bridgeRepoInstance);

    let bridgeServiceClass = require("../../src/services/bridgeService").default;
    let bridgeServiceInstance = Container.get(bridgeServiceClass);
    Container.set("BridgeService", bridgeServiceInstance);


  });
  afterEach(function() {
    sinon.restore();
    sandbox.restore();
  });


  it("bridgeController unit test using bridgeService mock", async function() {
    let body = { "name": "bridge-A1-B1", "code": "bridge-A1-B1", "floorAId": "FA1", "floorBId": "FB1" };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy()
    };

    let next: Partial<NextFunction> = () => {
    };

    let bridgeServiceInstance = Container.get("BridgeService");
    const bridgeServiceMock = sinon.mock(bridgeServiceInstance, "createBridge");

    bridgeServiceMock.expects("createBridge")
      .once()
      .withArgs(sinon.match({
        name: req.body.name,
        code: req.body.code,
        floorAId: req.body.floorAId,
        floorBId: req.body.floorBId
      }))
      .returns(Result.ok<IBridgeDTO>({
        "id": "123",
        "code": req.body.code,
        "name": req.body.name,
        "floorAId": req.body.floorAId,
        "floorBId": req.body.floorBId,
        "buildingAId": "A",
        "buildingBId": "B"
      }));

    const ctrl = new BridgeController(bridgeServiceInstance as IBridgeService);

    // Act
    await ctrl.createBridge(<Request>req, <Response>res, <NextFunction>next);

    // Assert
    bridgeServiceMock.verify();
    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({
      "id": "123",
      "code": req.body.code,
      "name": req.body.name,
      "floorAId": req.body.floorAId,
      "floorBId": req.body.floorBId,
      "buildingAId": "A",
      "buildingBId": "B"
    }));
  });

  it("bridgeController unit test using bridgeService stub", async function() {
    // Arrange
    let body = { "name": "bridge-A1-B1", "code": "bridge-A1-B1", "floorAId": "FA1", "floorBId": "FB1" };
    let req: Partial<Request> = {};

    req.body = body;
    let res: Partial<Response> = {
      json: sinon.spy()
    };

    let next: Partial<NextFunction> = () => {
    };

    let bridgeServiceInstance = Container.get("BridgeService");
    sinon.stub(bridgeServiceInstance, "createBridge").returns(Result.ok<IBridgeDTO>(
      {
        "id": "123",
        "code": req.body.code,
        "name": req.body.name,
        "floorAId": req.body.floorAId,
        "floorBId": req.body.floorBId,
        "buildingAId": "A",
        "buildingBId": "B"
      }));

    const ctrl = new BridgeController(bridgeServiceInstance as IBridgeService);

    // Act
    await ctrl.createBridge(<Request>req, <Response>res, <NextFunction>next);

    // Assert
    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({
      "id": "123",
      "code": req.body.code,
      "name": req.body.name,
      "floorAId": req.body.floorAId,
      "floorBId": req.body.floorBId,
      "buildingAId": "A",
      "buildingBId": "B"
    }));
  });

  it('given valid properties create valid Bridge instance', () => {
    const validProps = {
      id: "123",
      code: "A",
      name: "Edificio A - Administracao",
      floorAId: "123",
      floorBId: "123",
      buildingAId: "123",
      buildingBId: "123"
    };

    const bridge = Bridge.create(validProps);

    expect(bridge.isSuccess).to.be.true;
    expect(bridge.getValue()).to.be.an.instanceOf(Bridge);
  });

  it('given properties without floorId fails to create Bridge instance', () => {
    const invalidProps = {
      id: "123",
      code: "A",
      name: "Edificio A - Administracao",
      floorAId: "123",
      floorBId: "",
      buildingAId: "123",
      buildingBId: "123"
    };

    const bridge = Bridge.create(invalidProps);

    expect(bridge.isFailure).to.be.false;
    //expect(bridge.error).to.be.equal('floorBId is null or undefined');
  });

});
