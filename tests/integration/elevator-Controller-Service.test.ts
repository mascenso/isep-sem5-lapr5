import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IElevatorService from "../../src/services/IServices/IElevatorService";
import ElevatorController from "../../src/controllers/elevatorController";
import { IElevatorDTO } from '../../src/dto/IElevatorDTO';
import { Elevator } from '../../src/domain/elevator';
import { Floor } from '../../src/domain/floor';

describe('Integration test elevator controller -> service ', function () {
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

    it('elevatorController -> elevatorService integration test using elevatorRepo and elevator stub (createElevator)', async function () {
        // Arrange
        let body = { "code": 'Elev1', "floorId": '1', "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };
        let bodyFloor = { "buildingId": 'Building', "width": 10, "length": 10, "floorNumber": 2, "floorMap": [[]], "description": "Edificio muito alto." };
        let req: Partial<Request> = {};
        req.body = body;
        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let floorRepoInstance = Container.get("FloorRepo");
        let elevatorRepoInstance = Container.get("ElevatorRepo");
        let elevatorInstance = Container.get("Elevator");

        const floorRepoStub = sinon.stub(floorRepoInstance, "findByDomainId").returns(new Promise<Floor>((resolve, reject) => {
            resolve(Floor.create({
                "buildingId": bodyFloor.buildingId,
                "floorNumber": bodyFloor.floorNumber,
                "width": bodyFloor.width,
                "length": bodyFloor.length,
                "floorMap": bodyFloor.floorMap,
                "description": bodyFloor.description
            }).getValue())
        }));

        const elevatorRepoStub = sinon.stub(elevatorRepoInstance, "save").returns(new Promise<Elevator>((resolve, reject) => {
            resolve(Elevator.create({
                "code": req.body.code,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2,
                "floorId": req.body.floorId
            }).getValue())
        }));

        const elevatorStub = sinon.stub(elevatorInstance, "create").returns(Result.ok({
            "id": "123",
            "code": req.body.code,
            "coordX1": req.body.coordX1,
            "coordY1": req.body.coordX2,
            "coordX2": req.body.coordY1,
            "coordY2": req.body.coordY2,
            "floorId": req.body.floorId
        }));

        let elevatorServiceInstance = Container.get('ElevatorService');
        const ctrl = new ElevatorController(elevatorServiceInstance as IElevatorService);

        // Act
        await ctrl.createElevator(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({
            "id": "123",
            "code": req.body.code,
            "coordX1": req.body.coordX1,
            "coordY1": req.body.coordX2,
            "coordX2": req.body.coordY1,
            "coordY2": req.body.coordY2,
            "floorId": req.body.floorId
        }));
        sinon.assert.calledOnce(elevatorStub);
        sinon.assert.calledOnce(elevatorRepoStub);
        sinon.assert.calledOnce(floorRepoStub);
    });

    it('elevatorController -> elevatorService integration test using elevatorRepo and elevator stub (updateElevator)', async function () {
        // Arrange
        let body = { "code": 'Elev1', "floorId": '1', "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };
        let bodyFloor = { "buildingId": 'Building', "width": 10, "length": 10, "floorNumber": 2, "floorMap": [[]], "description": "Edificio muito alto." };
        let req: Partial<Request> = {};
        req.body = body;
        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let floorRepoInstance = Container.get("FloorRepo");
        let elevatorRepoInstance = Container.get("ElevatorRepo");
        let elevatorInstance = Container.get("Elevator");

        const elevatorRepoStub2 = sinon.stub(elevatorRepoInstance, "findByDomainId").returns(new Promise<Elevator>((resolve, reject) => {
            resolve(Elevator.create({
                "code": req.body.code,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2,
                "floorId": req.body.floorId
            }).getValue())
        }));

        const elevatorRepoStub = sinon.stub(elevatorRepoInstance, "save").returns(new Promise<Elevator>((resolve, reject) => {
            resolve(Elevator.create({
                "code": req.body.code,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2,
                "floorId": req.body.floorId
            }).getValue())
        }));


        let elevatorServiceInstance = Container.get('ElevatorService');
        const ctrl = new ElevatorController(elevatorServiceInstance as IElevatorService);

        // Act
        await ctrl.updateElevator(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({
            code: "Elev1",
            coordX1: 1,
            coordX2: 1,
            coordY1: 2,
            coordY2: 3,
            floorId: "1"
        }));

        sinon.assert.calledOnce(elevatorRepoStub);
        sinon.assert.calledOnce(elevatorRepoStub2);
    });


    it('elevatorController -> elevatorService integration test using elevatorRepo and Elevator stub (getAllElevators)', async function () {
        // Arrange
        let body = { "id": "123", "code": 'Elev1', "floorId": '1', "coordX1": 1, "coordY1": 2, "coordX2": 1, "coordY2": 3 };
        let req: Partial<Request> = {};
        req.body = body;
        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let elevatorRepoInstance = Container.get("ElevatorRepo");
        const getAllElevatorsStub = sinon.stub(elevatorRepoInstance, "getAllElevators").returns(new Promise<Elevator[]>((resolve, reject) => {
            resolve([Elevator.create({
                "code": req.body.code,
                "coordX1": req.body.coordX1,
                "coordY1": req.body.coordX2,
                "coordX2": req.body.coordY1,
                "coordY2": req.body.coordY2,
                "floorId": req.body.floorId
            }).getValue()])
        }));


        let elevatorServiceInstance = Container.get('ElevatorService');
        const ctrl = new ElevatorController(elevatorServiceInstance as IElevatorService);

        // Act
        await ctrl.getAllElevators(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(getAllElevatorsStub);
    });

});


