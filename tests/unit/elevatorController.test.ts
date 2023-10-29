import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';

import { Result } from '../../src/core/logic/Result';
import { IElevatorDTO } from '../../src/dto/IElevatorDTO';
import IElevatorService from '../../src/services/IServices/IElevatorService';
import ElevatorController from "../../src/controllers/elevatorController";
import Container from 'typedi';



describe('elevator controller', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        this.timeout(6000);
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

        let elevatorRepoClass = require("../../src/repos/elevatorRepo").default;
        let elevatorRepoInstance = Container.get(elevatorRepoClass);
        Container.set("ElevatorRepo", elevatorRepoInstance);

        let floorRepoClass = require("../../src/repos/floorRepo").default;
        let floorRepoInstance = Container.get(floorRepoClass);
        Container.set("FloorRepo", floorRepoInstance);

        let buildingInstance = require("../../src/domain/building").Building;
        Container.set("Building", buildingInstance);

        let floorInstance = require("../../src/domain/floor").Floor;
        Container.set("Floor", floorInstance);

        let elevatorInstance = require("../../src/domain/elevator").Elevator;
        Container.set("Elevator", elevatorInstance);

        let elevatorServiceClass = require("../../src/services/elevatorService").default;
        let elevatorServiceInstance = Container.get(elevatorServiceClass);
        Container.set("ElevatorService", elevatorServiceInstance);

        let floorServiceClass = require("../../src/services/floorService").default;
        let floorServiceInstance = Container.get(floorServiceClass);
        Container.set("FloorService", floorServiceInstance);

    });

    afterEach(function () {
        sinon.restore();
        sandbox.restore();
    });

    it('elevatorController unit test using elevatorService stub', async function () {
        // Arrange
        let body = {"code": 'Elev1', "floorId": 1, "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3};
  
        let req: Partial<Request> = {};
        req.body = body;
        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let elevatorServiceInstance = Container.get("ElevatorService");
        sinon.stub(elevatorServiceInstance, "createElevator").returns(Result.ok<IElevatorDTO>({
            "id": "123",
            "code": req.body.code,
            "floorId": req.body.floorId,
            "coordX1": req.body.coordX1,
            "coordY1": req.body.coordX2,
            "coordX2": req.body.coordY1,
            "coordY2": req.body.coordY2
        }));

        const ctrl = new ElevatorController(elevatorServiceInstance as IElevatorService);

        // Act
        await ctrl.createElevator(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({
            "id": "123",
            "code": req.body.code,
            "floorId": req.body.floorId,
            "coordX1": req.body.coordX1,
            "coordY1": req.body.coordX2,
            "coordX2": req.body.coordY1,
            "coordY2": req.body.coordY2
        }));
    });

    it('elevatorController unit test using elevatorService mock', async function () {
        // Arrange
        let body = {"code": 'Elev1', "floorId": 1, "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3};
        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let elevatorServiceInstance = Container.get("ElevatorService");
        const elevatorServiceMock = sinon.mock(elevatorServiceInstance, "createElevator")
        elevatorServiceMock.expects("createElevator")
            .once()
            .withArgs(sinon.match({ name: req.body.name }))
            .returns(Result.ok<IElevatorDTO>({
                "id": "123",
                "code": req.body.code,
                "floorId": req.body.floorId,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2
            }));

        const ctrl = new ElevatorController(elevatorServiceInstance as IElevatorService);

        // Act
        await ctrl.createElevator(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        elevatorServiceMock.verify();
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({
            "id": "123",
            "code": req.body.code,
            "floorId": req.body.floorId,
            "coordX1": req.body.coordX1,
            "coordY1": req.body.coordX2,
            "coordX2": req.body.coordY1,
            "coordY2": req.body.coordY2
        }));
    });

});


