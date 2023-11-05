import 'reflect-metadata';

import * as sinon from 'sinon';
import { expect } from 'chai';
import { NextFunction, Request } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import ElevatorService from "../../src/services/elevatorService";
import { Elevator } from '../../src/domain/elevator';

describe('elevator service', function () {
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

        let buildingInstance = require("../../src/domain/building-agg/building").Building;
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

    it('elevatorService unit test using elevator and elevatorRepo stubs (createElevator)', async function () {
        // Arrange
        let body = { "code": 'Elev1', "floorId": 1, "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };
        let req: Partial<Request> = {};
        req.body = body;

        let elevatorInstance = Container.get("Elevator");
        const elevatorStub = sinon.stub(elevatorInstance, "create").returns(Result.ok({
            "id": "123",
            "code": req.body.code,
            "floorId": req.body.floorId,
            "coordX1": req.body.coordX1,
            "coordY1": req.body.coordX2,
            "coordX2": req.body.coordY1,
            "coordY2": req.body.coordY2
        }));

        let elevatorRepoInstance = Container.get("ElevatorRepo");
        const elevatorRepoStub = sinon.stub(elevatorRepoInstance, "save").returns(new Promise<Elevator>((resolve, reject) => {
            resolve(Elevator.create({
                "code": req.body.code,
                "floorId": req.body.floorId,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2
            }).getValue())
        }));

        // Stub para IFloorRepo
        const floorRepoInstance = {
            save: sinon.stub(),
            findByDomainId: sinon.stub(),
        };
        Container.set("FloorRepo", floorRepoInstance);

        const service = new ElevatorService(Container.get("ElevatorRepo"), Container.get("FloorRepo"));

        const elevatorDTO = { "id": "123", "code": 'Elev1', "floorId": "1", "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };

        // Act
        await service.createElevator(elevatorDTO);

        // Assert
        sinon.assert.called(elevatorStub);
        sinon.assert.calledOnce(elevatorRepoStub);
        sinon.assert.calledWith(elevatorStub, sinon.match(elevatorDTO));

    });

    it('elevatorService unit test using elevator and elevatorRepo stubs (updateElevator)', async function () {
        // Arrange
        let body = { "code": 'Elev1', "floorId": 1, "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };
        let req: Partial<Request> = {};
        req.body = body;

        let elevatorRepoInstance = Container.get("ElevatorRepo");
        const elevatorStub = sinon.stub(elevatorRepoInstance, "findByDomainId").returns(new Promise<Elevator>((resolve, reject) => {
            resolve(Elevator.create({
                "code": req.body.code,
                "floorId": req.body.floorId,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2
            }).getValue())
        }));


        const elevatorRepoStub = sinon.stub(elevatorRepoInstance, "save").returns(new Promise<Elevator>((resolve, reject) => {
            resolve(Elevator.create({
                "code": req.body.code,
                "floorId": req.body.floorId,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2
            }).getValue())
        }));

        // Stub para IFloorRepo
        const floorRepoInstance = {
            save: sinon.stub(),
            findByDomainId: sinon.stub(),
        };
        Container.set("FloorRepo", floorRepoInstance);

        const service = new ElevatorService(Container.get("ElevatorRepo"), Container.get("FloorRepo"));

        const elevatorDTO = { "id": "123", "code": 'Elev1', "floorId": "1", "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };

        // Act
        await service.updateElevator(elevatorDTO);

        // Assert
        sinon.assert.called(elevatorStub);
        sinon.assert.calledOnce(elevatorRepoStub);
        sinon.assert.calledWith(elevatorStub, sinon.match(123));

    });

    it('elevatorService unit test using elevator and elevatorRepo stubs (getAllElevators)', async function () {
        // Arrange
        let body = { "code": 'Elev1', "floorId": 1, "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };
        let req: Partial<Request> = {};
        req.body = body;

        let elevatorRepoInstance = Container.get("ElevatorRepo");

        const elevatorRepoStub = sinon.stub(elevatorRepoInstance, "getAllElevators").returns(new Promise<Elevator[]>((resolve, reject) => {
            resolve([Elevator.create({
                "code": req.body.code,
                "floorId": req.body.floorId,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2
            }).getValue()])
        }));
        const buildingProps = {
            code: 'B001',
            maxWidth: 10,
            maxLength: 20,
            name: 'Sample Building',
            description: 'A test building',
        };

        // Stub para IFloorRepo
        const floorRepoInstance = {
            save: sinon.stub(),
            findByDomainId: sinon.stub(),
        };
        Container.set("FloorRepo", floorRepoInstance);

        const service = new ElevatorService(Container.get("ElevatorRepo"), Container.get("FloorRepo"));
        const elevatorDTO = { "id": "123", "code": 'Elev1', "floorId": "1", "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };

        // Act
        await service.getAllElevators();

        // Assert
        sinon.assert.calledOnce(elevatorRepoStub);

    });

});
