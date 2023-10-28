import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IBuildingService from "../../src/services/IServices/IBuildingService";
import BuildingController from "../../src/controllers/buildingController";
import {IBuildingDTO} from '../../src/dto/IBuildingDTO';
import { Building } from '../../src/domain/building';

describe('building controller', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		this.timeout(6000);
		Container.reset();
		let buildingSchemaInstance = require("../../src/persistence/schemas/buildingSchema").default;
		Container.set("buildingSchema", buildingSchemaInstance);

		let buildingRepoClass = require("../../src/repos/buildingRepo").default;
		let buildingRepoInstance = Container.get(buildingRepoClass);
		Container.set("BuildingRepo", buildingRepoInstance);

		let buildingInstance = require("../../src/domain/building").default;
		Container.set("Building", buildingInstance);

		let floorInstance = require("../../src/domain/floor").default;
		Container.set("Floor", floorInstance);

		let buildingServiceClass = require("../../src/services/buildingService").default;
		let buildingServiceInstance = Container.get(buildingServiceClass);
		Container.set("BuildingService", buildingServiceInstance);

    });

	afterEach(function() {
		sinon.restore();
		sandbox.restore();
	});

    it('buildingController unit test using buildingService stub (createBuilding)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingServiceInstance = Container.get("BuildingService");
		sinon.stub(buildingServiceInstance, "createBuilding").returns( Result.ok<IBuildingDTO>( {
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description} ));

		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.createBuilding(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description
		}));
	});

    it('buildingController unit test using buildingService mock (createBuilding)', async function () {		
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;

        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingServiceInstance = Container.get("BuildingService");		
		const buildingServiceMock = sinon.mock(buildingServiceInstance, "createBuilding")
		buildingServiceMock.expects("createBuilding")
			.once()
			.withArgs(sinon.match({name: req.body.name}))
			.returns(Result.ok<IBuildingDTO>( {
				"id":"123", 
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}));

		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.createBuilding(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		buildingServiceMock.verify();
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description
		}));
	});

	it('buildingController unit test using buildingService stub (updateBuilding)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingServiceInstance = Container.get("BuildingService");
		sinon.stub(buildingServiceInstance, "updateBuilding").returns( Result.ok<IBuildingDTO>( {
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description} ));

		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.updateBuilding(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description
		}));
	});

	it('buildingController unit test using buildingService stub (getAllBuildings)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingServiceInstance = Container.get("BuildingService");
		sinon.stub(buildingServiceInstance, "getAllBuildings").returns( Result.ok<IBuildingDTO[]>( [{
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description}]));

		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.getAllBuildings(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match([{ 
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description
		}]));
	});
});


